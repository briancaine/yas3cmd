open Sexplib.Std
open Cmdliner

type authentication = {
  access_key_id     : string option;
  secret_access_key : string option;
} [@@deriving sexp]

let make_authentication access_key_id secret_access_key =
  let string_or_none = function
    | ""        -> None
    | something -> Some something in
  {
    access_key_id     = string_or_none access_key_id;
    secret_access_key = string_or_none secret_access_key;
  }

let access_key_id =
  let doc = "AWS Access Key ID" in
  let env = Arg.env_var "AWS_ACCESS_KEY_ID" ~doc in
  Arg.(value &
       opt string "" &
       info ["access-key-id";]
            ~env
            ~docv:"ACCESS_KEY_ID"
            ~doc)

let secret_access_key =
  let doc = "AWS Secret Access Key" in
  let env = Arg.env_var "AWS_SECRET_ACCESS_KEY" ~doc in
  Arg.(value &
       opt string "" &
       info ["secret-access-key";]
            ~env
            ~docv:"SECRET_ACCESS_KEY"
            ~doc)

let copts_t =
  Term.(const make_authentication $ access_key_id $ secret_access_key)

let split_s3_uri uri =
  let open S3 in
  Uri.host_with_default uri |> Bucket.Name.of_string,
  Uri.path uri |> Object.Name.of_string
