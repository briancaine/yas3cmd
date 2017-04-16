open Printf
open Core.Std
open Cohttp
open Sexplib

open Yas3cmd_common

exception MissingFile of string
exception FileTooLarge

let max_put_size = Int64.of_int (5 * 1000 * 1000 * 1000) (* TODO: is this correct? *)

let put_buffer_size = (5 * 1000 * 1000) (* TODO: no clue what to put here *)

let simple_put { access_key_id; secret_access_key; } source target =

  let access_key_id = Option.value_exn access_key_id in
  let secret_access_key = Option.value_exn secret_access_key in

  let target = Uri.of_string target in

  let request_uri =
    Uri.make ~scheme:"https"
             ~host:(Uri.host_with_default target ^ ".s3.amazonaws.com")
             ~path:(Uri.path target ^ source)
             () in

  let source_stat = Unix.stat source in

  if Unix.(source_stat.st_size) >= max_put_size
  then raise FileTooLarge ;

  let content_md5 = Digest.(file source)
                    |> Cstruct.of_string
                    |> Nocrypto.Base64.encode
                    |> Cstruct.to_string in

  let request =
    Request.make ~meth:`PUT
                 ~version:`HTTP_1_1
                 ~headers:(
                   Header.of_list
                     [
                       "Date",
                       Time.(format (now ()) "%a, %d %b %Y %T +0000"
                                    ~zone:Core.Zone.utc);

                       "Content-Length",
                       Unix.(source_stat.st_size) |> Int64.to_string;

                       "Content-MD5", content_md5;
                     ])
                 request_uri in
  let request = S3.Authentication.authenticate
                  ~access_key_id ~secret_access_key
                  request in
  let headers = Request.headers request in

  let input_stream =
    Lwt_stream.from
      (let has_ran = ref false in
       fun () ->
       if !has_ran
       then Lwt.return None
       else let open Lwt_io in
            let%lwt ic  = open_file ~mode:Input source in
            let%lwt res = read ic in
            let%lwt ()  = close ic in
            has_ran    := true ;
            Lwt.return (Some res)) in

  let body = Cohttp_lwt_body.of_stream input_stream in

  Lwt_main.run
    (let%lwt (response, body) =
       Cohttp_lwt_unix.Client.put
         ~body
         ~headers
         request_uri in

     let%lwt body_contents = Cohttp_lwt_body.to_string body in

     if Response.status response <> `OK
     then
       (fprintf stderr "Failed to PUT object.\n" ;
        fprintf stderr "\n" ;
        fprintf stderr "%s\n"
                (Response.sexp_of_t response
                 |> Sexplib.Sexp.to_string_hum ~indent:2);
        fprintf stderr "%s\n" body_contents) ;

     Lwt.return ())

type complex_put_state = {
  state_filename : string;
  source         : string;
  target         : string;
} [@@deriving sexp]

let state_from_filename filename =
  let input = open_in filename in
  let state_sexp =
    try
      Sexp.input_sexp input
      |> complex_put_state_of_sexp
    with
    | exn ->
       In_channel.close input ;
       raise exn
  in
  {
    state_sexp with
    state_filename = filename;
  }

let complex_put auth state source target =
  printf "TODO: finish complex_put %s %s\n" source target

type put_opts = {
  state_filename : string option;
}

let put auth put_opts source target =
  if put_opts.state_filename = None
  then simple_put auth source target
  else
    complex_put auth
                (Option.value_exn put_opts.state_filename
                |> state_from_filename)
                source
                target

open Cmdliner

let make_put_opts state_filename =
  { state_filename; }

let put_opts =
  
  let state_file =
    let doc = "File in which to store Put state " ^
              "(so that it may be resumed later)" in
    Arg.(value &
         opt (some string) None &
         info ["state-file";]
              ~docv:"STATE_FILE"
              ~doc)
  in

  Term.(const make_put_opts $ state_file)

let source =
  let doc = "Source file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc)

let target =
  let doc = "Target S3 URL" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"TARGET" ~doc)

let cmd =
  let doc = "Put an object on a bucket" in
  let exits = Term.default_exits in
  let man = [] in
  Term.(const put $ copts_t $ put_opts $ source $ target),
  Term.info "put" ~doc ~sdocs:Manpage.s_common_options ~exits ~man
