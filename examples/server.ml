open Lwt.Infix
module Store = Irmin_unix.Git.Mem.KV (Irmin.Contents.String)
module Rpc =
  Irmin_rpc_unix.Make (Store) (Irmin_rpc_unix.Git_unix_endpoint_codec)

let main =
  Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
  Rpc.Server.serve ~secret_key:`Ephemeral (`TCP ("127.0.0.1", 9999)) repo
  >>= fun server ->
  Lwt_io.printl (Uri.to_string (Rpc.Server.uri server)) >>= fun () ->
  fst (Lwt.wait ())

let () = Lwt_main.run main
