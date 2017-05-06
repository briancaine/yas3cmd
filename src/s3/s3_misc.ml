open Core.Std

module Int = struct
  include Int

  let of_string_opt str =
    try Some (of_string str)
    with Failure _ -> None
end

module Cohttp_lwt_body = struct
  include Cohttp_lwt_body

  (* streams the contents of the file
   *)
  let of_filename ?len ?(pos=0L) ?(buffer_size=8192) filename =

    let buffer = String.create buffer_size in
    let%lwt ic = Lwt_io.(open_file ~mode:Input filename) in

    let%lwt file_length = Lwt_io.length ic in
    let len = Option.value len ~default:Int64.(file_length - pos) in

    let pos = ref pos in
    let end_pos = Int64.(!pos + len) in

    let%lwt () = Lwt_io.set_position ic !pos in

    let get_next () =
      if Int64.(!pos >= end_pos)
      then Lwt.return None
      else
        let amount_to_read =
          let open Int64 in
          if !pos + (of_int buffer_size) > end_pos
          then end_pos - !pos |> Int.of_int64_exn
          else buffer_size in
        try%lwt
          let%lwt () = Lwt_io.read_into_exactly
                         ic buffer 0
                         amount_to_read in
             (if amount_to_read = buffer_size
              then Some buffer
              else Some (String.sub ~pos:0 ~len:amount_to_read buffer))
             |> Lwt.return
        with
        | exc ->
           let%lwt () = Lwt_io.close ic in
           Lwt.fail exc in

    Lwt_stream.from get_next |> Lwt.return
end
