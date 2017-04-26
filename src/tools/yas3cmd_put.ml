open Printf
open Core.Std
open Cohttp
open Sexplib

open Yas3cmd_common

exception MissingFile of string
exception FileTooLarge

let file_size filename =
  let open Unix in
  (stat filename).st_size

let max_put_size = Int64.of_int (5 * 1000 * 1000 * 1000) (* TODO: is this correct? *)

let put_buffer_size = (10 * 1000 * 1000) (* TODO: no clue what to put here *)

let max_thread_count = 16 (* TODO: not sure yet... *)

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

type piece_success = {
  number : int;
  etag   : string;
} [@@deriving sexp]

type complex_put_state = {
  state_filename     : string;
  source             : string;
  target             : string;
  piece_size         : int;
  upload_id          : string;
  uploaded_pieces    : (int, piece_success) Map.Poly.t;
  pieces_in_progress : int Set.Poly.t;
} [@@deriving sexp]

let rec upload_id_of_xml xml =
  let open Xml in
  match xml with
  | Element ("UploadId", _, [PCData value]) -> Some value
  | Element (_, _, body) ->
     let rec iter = function
       | [] -> None
       | next :: rest ->
          match upload_id_of_xml next with
          | None -> iter rest
          | Some value -> Some value
     in
     iter body
  | _ -> None

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

let piece_count_of_file_size size piece_size =
  let div = size / piece_size in
  if div * piece_size = size
  then div
  else div + 1

let update_written_put_state put_state =
  let put_state_string = sexp_of_complex_put_state put_state
                         |> fun sexp -> Sexp.to_string_hum sexp in
  let put_state_string = put_state_string ^ "\n" in
  let temp_filename = put_state.state_filename ^ ".temp" in
  let desc = Unix.(openfile ~mode:[O_WRONLY; O_CREAT; O_EXCL;]
                            temp_filename) in
  if (Unix.single_write ~buf:put_state_string desc) = 0
  then failwith "Failed to write state, bailing.\n" ;
  Unix.close desc;
  Unix.rename temp_filename put_state.state_filename

let finish_complex_put auth put_state source target =
  let { access_key_id; secret_access_key; } = auth in
  let access_key_id = Option.value_exn access_key_id in
  let secret_access_key = Option.value_exn secret_access_key in

  let target = Uri.of_string target in

  let request_uri =
    Uri.make ~scheme:"https"
             ~host:(Uri.host_with_default target ^ ".s3.amazonaws.com")
             ~path:(Uri.path target ^ source)
             ~query:["uploadId", [put_state.upload_id];]
             () in

  let request =
    Request.make ~meth:`POST
                 ~version:`HTTP_1_1
                 ~headers:(
                   Header.of_list
                     [
                       "Date",
                       Time.(format (now ()) "%a, %d %b %Y %T +0000"
                                    ~zone:Core.Zone.utc);
                     ])
                 request_uri in
  let request = S3.Authentication.authenticate
                  ~access_key_id ~secret_access_key
                  request in
  let headers = Request.headers request in

  let sum_multipart_upload ~key ~data thus_far =
    thus_far ^
      (sprintf "<Part><PartNumber>%d</PartNumber><ETag>%s</ETag></Part>"
               (data.number + 1)
               data.etag)
  in

  let body =
    Cohttp_lwt_body.of_string
      ("<CompleteMultipartUpload>" ^
         (Map.Poly.fold ~init:""
                        ~f:sum_multipart_upload
                        put_state.uploaded_pieces) ^
           "</CompleteMultipartUpload>") in

  Lwt_main.run
    (let%lwt (response, body) =
       Cohttp_lwt_unix.Client.post
         ~body
         ~headers
         request_uri in

     let%lwt body_contents = Cohttp_lwt_body.to_string body in

     Lwt.return ())

let continue_complex_put auth put_state source target =

  let source_size = Int.of_int64_exn (file_size source) in
  let piece_count = piece_count_of_file_size source_size put_state.piece_size in
  let final_piece_size = source_size % put_state.piece_size in

  let handle_success put_state success =
    printf "Uploaded piece: #%d/%d\n" success.number piece_count;
    let put_state = {
      put_state with
      uploaded_pieces =
        Map.add put_state.uploaded_pieces
                ~key:success.number ~data:success;
      pieces_in_progress =
        Set.remove put_state.pieces_in_progress success.number;
    } in
    update_written_put_state put_state ;
    put_state
  in

  let next_piece_number uploaded in_progress =
    let uploaded_max = Map.max_elt uploaded
                       |> Option.value
                            ~default:(0, { number = 0; etag = ""; })
                       |> Tuple2.get1 in
    let in_progress_max = Set.max_elt in_progress |> Option.value ~default:0 in
    let start = Int.min uploaded_max in_progress_max in
    let rec iter start =
      if start >= piece_count
      then None
      else if not (Map.mem uploaded start) && not (Set.mem in_progress start)
      then Some start
      else iter (start + 1)
    in
    iter start in

  let upload_thread local_part_num =
    let remote_part_num = local_part_num + 1 in
    let%lwt data =
      let%lwt source = Lwt_unix.(openfile source [O_RDONLY] 0o640) in
      let data = String.create put_state.piece_size in
      let seek_target = local_part_num * put_state.piece_size in
      let%lwt pos = Lwt_unix.(lseek source seek_target SEEK_SET) in
      if pos <> seek_target
      then failwith "Failed to seek, bailing." ;
      let%lwt amount_read = Lwt_unix.read source data 0 (String.length data) in
      if amount_read <> (String.length data) &&
           local_part_num <> (piece_count - 1)
      then
        failwith "Failed to read whole chunk, bailing." ;
      let data = String.sub ~pos:0 ~len:amount_read data in
      Lwt.return data in

    let { access_key_id; secret_access_key; } = auth in
    let access_key_id = Option.value_exn access_key_id in
    let secret_access_key = Option.value_exn secret_access_key in

    let target = Uri.of_string target in

    let request_uri =
      Uri.make ~scheme:"https"
               ~host:(Uri.host_with_default target ^ ".s3.amazonaws.com")
               ~path:(Uri.path target ^ source)
               ~query:["partNumber", [Int.to_string remote_part_num];
                       "uploadId",   [put_state.upload_id];]
               () in

    let content_md5 = Digest.(string data)
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
                         String.length data |> Int.to_string;

                         "Content-MD5", content_md5;
                       ])
                   request_uri in
    let request = S3.Authentication.authenticate
                    ~access_key_id ~secret_access_key
                    request in
    let headers = Request.headers request in

    let body = Cohttp_lwt_body.of_string data in
    let%lwt (response, body) =
      Cohttp_lwt_unix.Client.put ~body ~headers request_uri in

    let%lwt body_contents = Cohttp_lwt_body.to_string body in

    if Response.status response <> `OK
    then
      (fprintf stderr "Failed to PUT object.\n" ;
       fprintf stderr "\n" ;
       fprintf stderr "%s\n"
               (Response.sexp_of_t response
                |> Sexplib.Sexp.to_string_hum ~indent:2);
       fprintf stderr "%s\n" body_contents) ;

    Lwt.return {
        number = local_part_num;
        etag = Header.get (Response.headers response) "etag"
               |> fun opt -> Option.value_exn opt;
      }
  in

  let rec allocate_new_threads put_state threads =
    if List.length threads >= max_thread_count
    then (put_state, threads)
    else
      let next_piece = next_piece_number
                         put_state.uploaded_pieces
                         put_state.pieces_in_progress in
      if next_piece = None
      then (put_state, threads)
      else
        let next_piece = Option.value_exn next_piece in
        let put_state = { put_state with pieces_in_progress = Set.add put_state.pieces_in_progress next_piece; } in
        allocate_new_threads put_state (upload_thread next_piece :: threads)
  in

  let rec main put_state threads =
    if threads = []
    then Lwt.return ()
    else (
    let%lwt (successes, threads) = Lwt.nchoose_split threads in

    let put_state = List.fold ~init:put_state ~f:handle_success successes in

    let (put_state, threads) = allocate_new_threads put_state threads in

    main put_state threads) in

  Lwt_main.run (
      let (put_state, threads) = allocate_new_threads put_state [] in
      main put_state threads)

type put_opts = {
  state_filename : string option;
}

let initiate_multipart_put auth put_opts source target =
  let state_filename = Option.value_exn put_opts.state_filename in
  let put_state = {
    state_filename;
    source;
    target;
    piece_size         = put_buffer_size;
    upload_id          = "";
    uploaded_pieces    = Map.Poly.empty;
    pieces_in_progress = Set.Poly.empty;
  } in
  let { access_key_id; secret_access_key; } = auth in
  let access_key_id = Option.value_exn access_key_id in
  let secret_access_key = Option.value_exn secret_access_key in

  let target = Uri.of_string target in

  let request_uri =
    Uri.make ~scheme:"https"
             ~host:(Uri.host_with_default target ^ ".s3.amazonaws.com")
             ~path:(Uri.path target ^ source)
             ~query:["uploads", [];]
             () in

  let request =
    Request.make ~meth:`POST
                 ~version:`HTTP_1_1
                 ~headers:(
                   Header.of_list
                     [
                       "Date",
                       Time.(format (now ()) "%a, %d %b %Y %T +0000"
                                    ~zone:Core.Zone.utc);
                     ])
                 request_uri in
  let request = S3.Authentication.authenticate
                  ~access_key_id ~secret_access_key
                  request in
  let headers = Request.headers request in

  Lwt_main.run
    (let%lwt (response, body) =
       Cohttp_lwt_unix.Client.post
         ~headers
         request_uri in

     let%lwt body_contents = Cohttp_lwt_body.to_string body in

     Lwt.return
       { put_state with
         upload_id = Xml.parse_string body_contents
                     |> upload_id_of_xml
                     |> fun opt -> Option.value_exn opt;
       })

let grab_state_or_init auth put_opts source target =
  let state_filename = Option.value_exn put_opts.state_filename in
  try
    let input = open_in state_filename in
    let res   = Sexp.input_sexp input
                |> complex_put_state_of_sexp in
    In_channel.close input ;
    {
      res with
      pieces_in_progress = Set.Poly.empty;
    }
  with
  | _ ->
    let res = initiate_multipart_put auth put_opts source target in
    update_written_put_state res ;
    res

let put auth put_opts source target =
  if put_opts.state_filename = None
  then simple_put auth source target
  else
    (try
        (continue_complex_put auth
                              (grab_state_or_init auth put_opts source target)
                              source
                              target ;
         finish_complex_put auth
                            (grab_state_or_init auth put_opts source target)
                            source
                            target)
      with obj ->
           match obj with
           | Ssl.Write_error _ ->
              raise obj;)

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
