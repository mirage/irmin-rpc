open Lwt.Infix
open Cmdliner

let config path =
  let head = Git.Reference.of_string "refs/heads/master" in
  Irmin_git.config ~head path

let run host port root contents store =
  let c = Irmin_unix.Cli.mk_contents contents in
  let (module Store) = Irmin_unix.Cli.mk_store store c in
  let module Rpc = Irmin_rpc_unix.Make(Store) in
  let p =
    Store.Repo.v (config root) >>= fun repo ->
    Rpc.Server.create ~secret_key:`Ephemeral (`TCP (host, port)) repo >>= fun server ->
    Printf.printf "Serving on: %s" (Uri.to_string (Rpc.Server.uri server));
    Rpc.Server.run server
  in Lwt_main.run p

let host =
  let doc = "Server address" in
  Arg.(value & opt string "127.0.0.1" & info ["a"; "address"] ~docv:"HOST" ~doc)

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 9998 & info ["p"; "port"] ~docv:"PORT" ~doc)

let contents =
  let doc = "Content type" in
  Arg.(value & opt string "string" & info ["c"; "contents"] ~docv:"CONTENTS" ~doc)

let store =
  let doc = "Store type" in
  Arg.(value & opt string "git" & info ["s"; "store"] ~docv:"STORE" ~doc)

let root =
  let doc = "Store location" in
  Arg.(value & opt string "/tmp/irmin" & info ["root"] ~docv:"PATH" ~doc)

let main_t = Term.(const run $ host $ port $ root $ contents $ store)
let () = Term.exit @@ Term.eval (main_t, Term.info "irmin-rpc")

