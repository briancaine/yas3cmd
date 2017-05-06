open Core.Std
open Cohttp

exception InvalidObjectName of string

type put_exception = {
  request       : Request.t;
  response      : Response.t;
  response_body : string;
} [@@deriving sexp]

exception PutFailure of put_exception

open S3_misc

module Name = struct
  type t = string

  let is_valid str =
    String.length str <= 1024

  let special_handling_chars =
    [
      '&';
      '$';
      '@';
      '=';
      ';';
      ':';
      '+';
      ' ';
      ',';
      '?';
    ] @ List.map ~f:Char.of_int_exn ((0 -- 31) @ [127])

  let needs_special_handling str =
    List.find ~f:(String.contains str) special_handling_chars <> None

  let should_be_avoided_chars =
    [
      '\\';
      '{';
      '^';
      '}';
      '%';
      '`';
      ']';
      '"';
      '\'';
      '>';
      '[';
      '~';
      '<';
      '#';
      '|';
    ] @
      (* todo:

         is this correct? I know ascii characters 7 bits and below are kosher
         utf8, but what about for that last bit?

         probably not? *)
      List.map ~f:Char.of_int_exn (128 -- 255)

  let should_be_avoided str =
    List.find ~f:(String.contains str) should_be_avoided_chars <> None

  let of_string item =
    if not (is_valid item)
    then raise (InvalidObjectName item) ;
    item

  let to_string (item : t) = item

  let add_to_https_uri t uri =
    Uri.with_path uri t

end

let kb = 1024
let mb = kb * 1024
let gb = mb * 1024
let tb = gb * 1024

(*

https://aws.amazon.com/s3/faqs/#billing_anchor
https://docs.aws.amazon.com/AmazonS3/latest/dev/qfacts.html

*)

(* no clue where I got this... *)
let max_single_put_size = Int64.of_int (5 * gb)

let now_time_str () =
  let open Time in
  format (now ()) "%a, %d %b %T %T +0000" ~zone:Core.Zone.utc

let standard_request uri =
  Request.make ~version:`HTTP_1_1
               ~headers:(Header.of_list ["Date", now_time_str()])
               uri

let update_request_with_body_str request data =
  let content_md5 = Digest.(string data)
                    |> Cstruct.of_string
                    |> Nocrypto.Base64.encode
                    |> Cstruct.to_string in
  Request.{
    request with
    headers = Header.add_list
                request.headers
                [
                  "Content-Length", String.length data |> Int.to_string;
                  "Content-MD5", content_md5;
                ]
  }

let execute_request_with_body_str request data =
  let body = Cohttp_lwt_body.of_string data in
  let open Request in
  match request.meth with
  | `PUT ->
     S3_cohttp_lwt_unix.Client.put
       ~body
       ~headers:request.headers
       (uri request)
  | _ -> failwith "TODO: add other methods"

let put ~access_key ~bucket_name ~object_name data =

  let%lwt data = Cohttp_lwt_body.to_string data in

  let request_uri = S3_bucket.Name.to_https_uri bucket_name
                    |> Name.add_to_https_uri object_name in

  let request = standard_request request_uri in
  let request = Request.{ request with meth = `PUT } in
  let request = update_request_with_body_str request data in
  let request = S3_authentication.authenticate request access_key in

  let%lwt (response, body, _) = execute_request_with_body_str request data in

  let%lwt response_body       = Cohttp_lwt_body.to_string body in

  if Response.status response <> `OK
  then PutFailure {
           request;
           response;
           response_body;
         }
       |> Lwt.fail
  else Lwt.return ()
