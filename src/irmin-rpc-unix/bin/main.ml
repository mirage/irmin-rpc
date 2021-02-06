open Cmdliner
open Lwt.Infix

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let config path =
  print_endline path;
  let head = Git.Reference.of_string "refs/heads/master" |> Result.get_ok in
  Irmin_git.config ~head path

let run (Irmin_unix.Resolver.S ((module Store), store, _)) host port secret_key
    address_file insecure =
  let module Rpc =
    Irmin_rpc_unix.Make
      (Store)
      (Irmin_rpc.Config.Remote.None (Store))
      (Irmin_rpc.Config.Pack.None (Store))
  in
  let secret_key =
    match secret_key with Some key -> `File key | None -> `Ephemeral
  in
  let secure = not insecure in
  let p =
    store >>= fun store ->
    Rpc.Server.serve ~secure ~secret_key (`TCP (host, port)) (Store.repo store)
    >>= fun server ->
    let () =
      match address_file with
      | Some f ->
          let f = open_out f in
          output_string f (Uri.to_string (Rpc.Server.uri server));
          close_out f
      | None ->
          Printf.printf "Serving on: %s\n%!"
            (Uri.to_string (Rpc.Server.uri server))
    in
    fst (Lwt.wait ())
  in
  Lwt_main.run p

let host =
  let doc = "Server address" in
  Arg.(
    value & opt string "127.0.0.1" & info [ "a"; "address" ] ~docv:"HOST" ~doc)

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 9998 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let secret_key =
  let doc = "Secret key" in
  Arg.(
    value
    & opt (some string) None
    & info [ "k"; "secret-key" ] ~docv:"FILENAME" ~doc)

let address_file =
  let doc = "Write address to file" in
  Arg.(
    value
    & opt (some string) None
    & info [ "f"; "address-file" ] ~docv:"FILENAME" ~doc)

let insecure =
  let doc = "Disable SSL and other security features" in
  Arg.(value & flag & info [ "insecure" ] ~doc)

let main_t =
  Term.(
    const run
    $ Irmin_unix.Resolver.store
    $ host
    $ port
    $ secret_key
    $ address_file
    $ insecure)

let () = Term.exit @@ Term.eval (main_t, Term.info "irmin-rpc")
