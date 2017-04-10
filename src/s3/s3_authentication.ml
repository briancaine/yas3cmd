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

type canonicalization_state = {
  finished : (string * string) list;
  last     : (string * string) option;
} [@@deriving sexp]

let canonicalization_to_list { finished; last; } =
  finished @
  (if last = None
   then []
   else [Option.value_exn last])

let fold_multiline_header (key, value) =
  key,
  Str.global_replace (Str.regexp "[\t ]*\n[\t ]*") " " value

let merge_amz_headers state header =
  let (key, value) = header in
  match state.last with
  | Some (last_key, last_value) ->
     if last_key = key
     then {
       state with
       last = Some (last_key, last_value ^ "," ^ value)
     }
     else {
       finished = state.finished @ [Option.value_exn state.last];
       last = Some header;
     }
  | None -> { state with last = Some header; }

let canonicalized_amz_headers req =
  Request.headers req
  |> Header.to_list
  |> List.map    ~f:(fun (name, value) -> String.lowercase name, value)
  |> List.filter ~f:(fun (name, _) ->
                     String.is_prefix ~prefix:"x-amz" name &&
                     name <> "x-amz-date")
  |> List.map    ~f:fold_multiline_header
  |> List.fold   ~init:{ finished = []; last = None; }
                 ~f:merge_amz_headers
  |> canonicalization_to_list
  |> List.map    ~f:(fun (key, value) -> key ^ ":" ^ value ^ "\n")
  |> String.concat

let canonicalized_resource req =

  let acc_prefix =
    let host       = header_value_exn "host" req InvalidHost in
    let host_uri   = Uri.of_string ("http://" ^ host) in
    let host_parts = String.(lowercase host |> String.split ~on:'.') in
    match host_parts with
    | [virtual_host; "s3"; "amazonaws"; "com";] -> "/" ^ virtual_host
    | [              "s3"; "amazonaws"; "com";] -> ""
    | _ -> "/" ^ (Uri.host_with_default host_uri) in

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

let request_date req =
  let headers = Request.headers req in
  match Header.get headers "x-amz-date" with
  | Some value -> value
  | None ->
     match Header.get headers "date" with
     | Some value -> value
     | None -> raise MissingDate

let string_to_sign req =
  (Request.meth req |> Code.string_of_method)       ^ "\n" ^
  (header_value_opt "Content-MD5"  req)             ^ "\n" ^
  (header_value_opt "Content-Type" req)             ^ "\n" ^
  (request_date req)                                ^ "\n" ^
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
