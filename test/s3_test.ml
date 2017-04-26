open OUnit2
open Printf
open Core.Std

let program = Conf.make_exec "s3_test"

module Authentication = struct

  (* some of the are taken from
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
      [
        authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Date", "Tue, 27 Mar 2007 19:36:42 +0000";])
            (Uri.of_string
               "https://johnsmith.s3.amazonaws.com/photos/puppy.jpg"))

         "AWS AKIAIOSFODNN7EXAMPLE:bWq2s1WEIj+Ydj0vQ697zp+IXMU=";

       authenticate_req
         (Request.make
            ~meth:`PUT
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Content-Type",   "image/jpeg";
                         "Content-Length", "94328";
                         "Date",           "Tue, 27 Mar 2007 21:15:45 +0000";])
            (Uri.of_string
               "https://johnsmith.s3.amazonaws.com/photos/puppy.jpg"))

         "AWS AKIAIOSFODNN7EXAMPLE:MyyxeRY7whkBe+bq8fHCL/2kKUg=";

       authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Date",       "Tue, 27 Mar 2007 19:42:41 +0000";
                         "User-Agent", "Mozilla/5.0";])
            (Uri.of_string
               ("https://johnsmith.s3.amazonaws.com/?" ^
                "prefix=photos&max-keys=50&marker=puppy")))

         "AWS AKIAIOSFODNN7EXAMPLE:htDYFYduRNen8P9ZfE/s9SuKy0U=";

       authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Date",       "Tue, 27 Mar 2007 19:44:46 +0000";
                         "User-Agent", "Mozilla/5.0";])
            (Uri.of_string
               "https://johnsmith.s3.amazonaws.com/?acl"))

         "AWS AKIAIOSFODNN7EXAMPLE:c2WLPFtWHVgbEmeEG93a4cG37dM=";

       authenticate_req
         (Request.make
            ~meth:`DELETE
            ~version:`HTTP_1_1
            ~headers:(Header.of_list
                        ["Date",       "Tue, 27 Mar 2007 21:20:27 +0000";
                         "User-Agent", "dotnet";
                         "X-AMZ-Date", "Tue, 27 Mar 2007 21:20:26 +0000";])
            (Uri.of_string
               "https://s3.amazonaws.com/johnsmith/photos/puppy.jpg"))

         "AWS AKIAIOSFODNN7EXAMPLE:lx3byBScXR6KzyMaifNkardMwNk=";

       authenticate_req
         (Request.make
            ~meth:`PUT
            ~version:`HTTP_1_1
            ~headers:(
              Header.of_list
                ["Date", "Tue, 27 Mar 2007 21:06:08 +0000";
                 "User-Agent",                   "curl/7.15.5";
                 "x-amz-acl",                    "public-read";
                 "content-type",                 "application/x-download";
                 "Content-MD5",                  "4gJE4saaMU4BqNR0kLY+lw==";
                 "X-Amz-Meta-ReviewedBy","joe@johnsmith.net,jane@johnsmith.net";
                 "X-Amz-Meta-FileChecksum",      "0x02661779";
                 "X-Amz-Meta-ChecksumAlgorithm", "crc32";
                 "Content-Disposition",
                   "attachment; filename=database.dat";
                 "Content-Encoding",             "gzip";
                 "Content-Length",               "5913339";])
            (Uri.of_string
               "http://static.johnsmith.net:8080/db-backup.dat.gz"))

         "AWS AKIAIOSFODNN7EXAMPLE:ilyl83RwaSoYIEdixDQcA4OnAnc=";

       authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(
              Header.of_list
                ["Date", "Wed, 28 Mar 2007 01:29:59 +0000";])
            (Uri.of_string "https://s3.amazonaws.com/"))

         "AWS AKIAIOSFODNN7EXAMPLE:qGdzdERIC03wnaRNKh6OqZehG9s=";

       (* ocaml-uri doesn't let you have lowercase characters for the hex in
          percent encodings, so this isn't exactly the test in the
          aws documentation*)
       authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(
              Header.of_list
                ["Date", "Wed, 28 Mar 2007 01:49:49 +0000";])
            (Uri.of_string
               ("https://s3.amazonaws.com" ^
                "/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re")))

         "AWS AKIAIOSFODNN7EXAMPLE:81VEw/Bc3GDt/k65Xrrk3AdfI4c=";

       (* misc tests to get code coverage *)

       ("Authorization already exists" >::
          fun ctxt ->
          assert_raises ~msg:"Authorization already exists"
                        S3.Authentication.AlreadyAuthenticated
                        (fun () ->
                         S3.Authentication.authenticate
                           (Request.make
                              ~meth:`GET
                              ~version:`HTTP_1_1
                              ~headers:(
                                Header.of_list [
                                    "Authorization", "fff";
                                    "Date", "date";
                                    "x-amz-foo", "fff";
                                    "x-amz-foo", "aaa";
                                    "x-amz-foo", "rrr";
                                  ])
                              (Uri.of_string "/foo?acl=foo&location=bar"))
                        ~access_key_id ~secret_access_key));

       ("Missing date" >::
          fun ctxt ->
          assert_raises ~msg:"Date missing"
                        S3.Authentication.MissingDate
                        (fun () ->
                         S3.Authentication.authenticate
                           (Request.make
                              ~meth:`GET
                              ~version:`HTTP_1_1
                              ~headers:(
                                Header.of_list [
                                    "x-amz-foo", "fff";
                                    "x-amz-foo", "aaa";
                                    "x-amz-foo", "rrr";
                                  ])
                              (Uri.of_string "/foo?acl=foo&location=bar"))
                        ~access_key_id ~secret_access_key));

(*
       authenticate_req
         (Request.make
            ~meth:`GET
            ~version:`HTTP_1_1
            ~headers:(
              Header.of_list
                ["Date", "Wed, 28 Mar 2007 01:49:49 +0000";
                 
])
            (Uri.of_string
               ("https://s3.amazonaws.com" ^
                "/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re")))

         "AWS AKIAIOSFODNN7EXAMPLE:81VEw/Bc3GDt/k65Xrrk3AdfI4c=";
 *)

      ]

end

module Bucket = struct

  let test_name name valid =
    let msg = sprintf "Bucket name \"%s\" is %s"
                      name
                      (if valid then "valid" else "invalid") in
    msg >::
      fun ctxt ->
      assert_equal ~ctxt ~msg valid (S3.Bucket.Name.is_valid name)

  let test =
    "Bucket" >:::
      [
        "Names" >:::
          [test_name "foo" true;
           test_name (String.make 63 'a') true;
           test_name (String.make 64 'a') false;
           test_name ".foo" false;
           test_name "foo." false;
           test_name "foo.doo" true;
           test_name "foo..doo" false;
           test_name "192.168.5.4" false;
           test_name "foo-doo.o.o.o.o.o.o.o.o" true;

           ("To/From string" >::
              fun ctxt ->
              assert_equal ~ctxt ~msg:"of_string"
                           "foo"
                           S3.Bucket.Name.(of_string "foo" |> to_string));
           ("To/From string (failure)" >::
              fun ctxt ->
              assert_raises ~msg:"Bad name conversion"
                            (S3.Bucket.InvalidBucketName "foo..bar")
                            (fun () ->
                             S3.Bucket.Name.(
                               of_string "foo..bar" |> to_string)));
          ]
      ]

end

let test =
  "S3" >:::
    [Authentication.test;
     Bucket.test;
    ]

let () = run_test_tt_main test
