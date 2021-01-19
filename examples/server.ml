open Lwt.Infix
open Common

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let main =
  Store.Repo.v (Irmin_pack.config "db") >>= fun repo ->
  Rpc.Server.serve ~secret_key:`Ephemeral (`TCP ("127.0.0.1", 9999)) repo
  >>= fun server ->
  Lwt_io.printl (Uri.to_string (Rpc.Server.uri server)) >>= fun () ->
  fst (Lwt.wait ())

let () = Lwt_main.run main
