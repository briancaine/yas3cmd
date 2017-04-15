(* OASIS_START *)
(* OASIS_STOP *)
let () =
  dispatch
    (MyOCamlbuildBase.dispatch_combine
       [MyOCamlbuildBase.dispatch_default conf package_default;
        Bisect_ppx_plugin.dispatch])
