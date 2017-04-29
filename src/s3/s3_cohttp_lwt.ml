open Cohttp
open Lwt

module Body = Cohttp_lwt_body

module type IO = S.IO with type 'a t = 'a Lwt.t

module S = Cohttp_lwt_s

module type Client = S.Client
module type Server = S.Server
module type Net = S.Net

module Make_request(IO:IO) = struct
  include Cohttp.Request
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module Make_response(IO:IO) = struct
  include Cohttp.Response
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module Request = Cohttp.Request
module Response = Cohttp.Response

module Make_client
    (IO:IO)
    (Net:Cohttp_lwt_s.Net with module IO = IO) = struct

  include Cohttp_lwt.Make_client(IO)(Net)

  module Response = Make_response(IO)
  module Request = Make_request(IO)

  let read_response ~closefn ic oc meth =
    Response.read ic >>= begin function
    | `Invalid reason ->
      Lwt.fail (Failure ("Failed to read response: " ^ reason))
    | `Eof -> Lwt.fail (Failure "Client connection was closed")
    | `Ok res -> begin
        let has_body = match meth with
          | `HEAD -> `No
          | _ -> Response.has_body res
        in
        match has_body with
        | `Yes | `Unknown ->
          let reader = Response.make_body_reader res ic in
          let stream = Body.create_stream Response.read_body_chunk reader in
          let closefn = closefn in
          Lwt_stream.on_terminate stream closefn;
          let gcfn st = closefn () in
          Gc.finalise gcfn stream;
          let body = Body.of_stream stream in
          return (res, body)
        | `No -> closefn (); return (res, `Empty)
      end
    end
    |> fun t ->
    Lwt.on_cancel t closefn;
    Lwt.on_failure t (fun _exn -> closefn ());
    t

  let is_meth_chunked = function
    | `HEAD -> false
    | `GET -> false
    | `DELETE -> false
    | _ -> true

  let call ?(ctx=default_ctx) ?headers ?(body=`Empty) ?chunked
           meth uri =
    let headers = match headers with None -> Header.init () | Some h -> h in
    Net.connect_uri ~ctx uri >>= fun (conn, ic, oc) ->
    let closefn () = Net.close ic oc in
    let chunked =
      match chunked with
      | None -> is_meth_chunked meth
      | Some v -> v in
    let sent = match chunked with
      | true ->
         let req = Request.make_for_client ~headers ~chunked meth uri in
         Request.write
           (fun writer ->
            Body.write_body (Request.write_body writer) body) req oc
      | false ->
         (* If chunked is not allowed, then obtain the body length and
           insert header *)
         Body.length body >>= fun (body_length, buf) ->
         let req =
           Request.make_for_client ~headers ~chunked ~body_length meth uri
         in
         Request.write
           (fun writer ->
            Body.write_body (Request.write_body writer) buf) req oc
    in
    let sent =
      let%lwt () = sent in
      Lwt.return `Sent
    in
    let received =
      let%lwt (response, body) = read_response ~closefn ic oc meth in
      Lwt.return (`Received (response, body, `RequestIncomplete))
    in
    match%lwt sent <?> received with
    | `Sent ->
       let%lwt `Received (response, body, _) = received in
       Lwt.return (response, body, `RequestComplete)
    | `Received result -> Lwt.return result

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri
  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri
  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri
  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri
  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri

end
