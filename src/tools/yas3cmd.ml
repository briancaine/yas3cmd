open Printf
open Cmdliner

let help_secs = [
]

let default_cmd =
  let doc = "yet another s3cmd" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Yas3cmd_common.copts_t)),
  Term.info "yas3cmd" ~version:"v0.0.1" ~doc ~sdocs ~exits ~man

let cmds = [Yas3cmd_put.cmd;]

let () =
  Term.(exit @@ eval_choice default_cmd cmds)
