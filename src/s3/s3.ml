module Authentication = S3_authentication

module Bucket = S3_bucket

module Cohttp_lwt = S3_cohttp_lwt

module Cohttp_lwt_unix = struct
  module Client = struct
    include Cohttp_lwt.Make_client (Cohttp_lwt_unix_io)(Cohttp_lwt_unix_net)
    let custom_ctx = Cohttp_lwt_unix_net.init
  end
end

module Misc = S3_misc
