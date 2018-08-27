open Lwt.Infix
open Cmdliner

let config path =
  let head = Git.Reference.of_string "refs/heads/master" in
  Irmin_git.config ~head path

let run host port root contents store secret_key address_file =
  let c = Irmin_unix.Cli.mk_contents contents in
  let (module Store) = Irmin_unix.Cli.mk_store store c in
  let module Rpc = Irmin_rpc_unix.Make(Store) in
  let secret_key =
    match secret_key with
    | Some key -> `File key
    | None -> `Ephemeral
  in
  let p =
    Store.Repo.v (config root) >>= fun repo ->
    Rpc.Server.create ~secret_key (`TCP (host, port)) repo >>= fun server ->
    let () = match address_file with
      | Some f ->
          let f = open_out f in
          output_string f (Uri.to_string (Rpc.Server.uri server));
          close_out f
      | None ->
          Printf.printf "Serving on: %s\n%!" (Uri.to_string (Rpc.Server.uri server))
    in
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

let secret_key =
  let doc = "Secret key" in
  Arg.(value & opt (some string) None & info ["k"; "secret-key"] ~docv:"FILENAME" ~doc)

let address_file =
  let doc = "Write address to file" in
  Arg.(value & opt (some string) None & info ["f"; "address-file"] ~docv:"FILENAME" ~doc)

let main_t = Term.(const run $ host $ port $ root $ contents $ store $ secret_key $ address_file)
let () = Term.exit @@ Term.eval (main_t, Term.info "irmin-rpc")

