open OUnit2
open Printf
open Core.Std

let program = Conf.make_exec "s3_test"

module Authentication = struct

  (* taken from
     https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html
   *)

  open Cohttp

  let access_key_id     = "AKIAIOSFODNN7EXAMPLE"
  let secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"

  let req_as_string req =
    Request.sexp_of_t req |> Sexp.to_string

  let authenticate_req req auth =
    (req_as_string req) >::
      fun ctxt ->
      let msg = sprintf "Failed to authentication request: %s"
                        (req_as_string req) in
      let req = S3.Authentication.authenticate
                  req ~access_key_id ~secret_access_key in
      assert_equal ~ctxt ~msg auth
                   Request.(Header.get req.headers "Authorization"
                            |> Option.value ~default:"")

  let test =
    "Authentication" >:::
      [authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Date", "Tue, 27 Mar 2007 19:36:42 +0000";])
            (Uri.of_string
               "https://johnsmith.s3.amazonaws.com/photos/puppy.jpg"))
         "AWS AKIAIOSFODNN7EXAMPLE:bWq2s1WEIj+Ydj0vQ697zp+IXMU="]

end

let test =
  "S3" >:::
    [Authentication.test]

let () = run_test_tt_main test
