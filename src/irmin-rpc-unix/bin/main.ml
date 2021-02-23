open Cmdliner
open Lwt.Infix

let () =
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (Logs_fmt.reporter ())

let http_server (type x) (module Store : Irmin.S with type repo = x) (_repo : x)
    _conn _req body =
  Cohttp_lwt.Body.drain_body body >>= fun () ->
  Cohttp_lwt_unix.Server.respond_string ~body:"OK" ~status:`OK ()

let run (Irmin_unix.Resolver.S ((module Store), store, _)) host port secret_key
    address_file insecure max_tx =
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
    Rpc.Server.serve ?max_tx ~secure ~secret_key
      (`TCP (host, port))
      (Store.repo store)
    >>= fun server ->
    let () =
      match address_file with
      | Some f ->
          let f = open_out f in
          output_string f (Uri.to_string (Rpc.Server.uri server));
          close_out f
      | None ->
          Logs.app (fun l -> l "%s" (Uri.to_string (Rpc.Server.uri server)))
    in
    let http =
      Cohttp_lwt_unix.Server.make
        ~callback:(http_server (module Store) (Store.repo store))
        ()
    in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port (port + 1))) http
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

let max_tx =
  let doc = "Maximum number of open transactions per client" in
  Arg.(value & opt (some int) None & info [ "x"; "max-tx" ] ~docv:"MAX" ~doc)

let main_t =
  Term.(
    const run
    $ Irmin_unix.Resolver.store
    $ host
    $ port
    $ secret_key
    $ address_file
    $ insecure
    $ max_tx)

let () = Term.exit @@ Term.eval (main_t, Term.info "irmin-rpc")
