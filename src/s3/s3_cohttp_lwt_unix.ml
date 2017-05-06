module Cohttp_lwt = S3_cohttp_lwt

module Client = struct
  include Cohttp_lwt.Make_client (Cohttp_lwt_unix_io)(Cohttp_lwt_unix_net)
  let custom_ctx = Cohttp_lwt_unix_net.init
end
