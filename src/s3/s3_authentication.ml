open Cohttp
open Printf
open Nocrypto.Hash
open Core.Std

exception InvalidHost
exception AlreadyAuthenticated
exception MissingDate

let canonical_subresources =
  List.map ~f:String.lowercase
           ["acl";
            "lifecycle";
            "location";
            "logging";
            "notification";
            "partNumber";
            "policy";
            "requestPayment";
            "torrent";
            "uploadId";
            "uploads";
            "versionId";
            "versioning";
            "versions";
            "website";]

let header_value_opt name req =
  Request.headers req
  |> fun headers -> Header.get headers name
  |> Option.value ~default:""

let header_value_exn name req exn =
  Request.headers req
  |> fun headers ->
  match Header.get headers name with
  | None       -> raise exn
  | Some value -> value

let canonicalized_amz_headers req =
  fprintf stderr "DEBUG: Not finished yet\n%!";
  "" (* debug finish *)

let canonicalized_resource req =

  let acc_prefix =
    let host       = header_value_exn "host" req InvalidHost in
    let host_parts = String.(lowercase host |> String.split ~on:'.') in
    match host_parts with
    | [              "s3"; "amazonaws"; "com";] -> ""
    | [virtual_host; "s3"; "amazonaws"; "com";] -> "/" ^ virtual_host
    | _                                         -> raise InvalidHost in

  let resource_path = Request.uri req |> Uri.path in

  let query =
    Request.uri req
    |> Uri.query
    |> List.filter
         ~f:(fun (arg_name, _) -> List.mem canonical_subresources arg_name)
    |> List.sort
         ~cmp:(fun (a_name, _) (b_name, _) -> String.compare a_name b_name) in

  Uri.make
    ~path:(acc_prefix ^ (if resource_path = "" then "/" else resource_path))
    ~query
    ()
  |> Uri.to_string

let string_to_sign req =
  (Request.meth req |> Code.string_of_method)       ^ "\n" ^
  (header_value_opt "Content-MD5"  req)             ^ "\n" ^
  (header_value_opt "Content-Type" req)             ^ "\n" ^
  (header_value_exn "Date"         req MissingDate) ^ "\n" ^
  canonicalized_amz_headers req ^
  canonicalized_resource req

let authenticate req ~access_key_id ~secret_access_key =
  if   Request.(Header.get req.headers) "Authorization" <> None
  then raise AlreadyAuthenticated ;

  Request.{
    req with
    headers =
      Header.add
        req.headers
        "Authorization"
        (sprintf "AWS %s:%s"
                 access_key_id
                 Cstruct.(string_to_sign req
                          |> of_string
                          |> SHA1.hmac ~key:(of_bytes secret_access_key)
                          |> to_string
                          |> B64.encode))
  }
