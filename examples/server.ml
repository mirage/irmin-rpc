open Lwt.Infix

module Store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Rpc = Irmin_rpc_unix.Make(Store)(struct
  let remote = Store.remote
end)

let main =
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
    Rpc.Server.create ~secret_key:`Ephemeral (`TCP ("127.0.0.1", 9999)) repo >>= fun server ->
    Lwt_io.printl (Uri.to_string (Rpc.Server.uri server)) >>= fun () ->
    Rpc.Server.run server

let () = Lwt_main.run main
