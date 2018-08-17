open Lwt.Infix

module Store = Irmin_mem.KV(Irmin.Contents.String)
module Rpc = Irmin_rpc_unix.Make(Store)

let main =
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
    Rpc.Server.create ~secret_key:`Ephemeral (`TCP ("127.0.0.1", 9999)) repo >>= fun server ->
    Printf.printf "Serving on: %s" (Uri.to_string (Rpc.Server.uri server));
    Rpc.Server.run server

let () = Lwt_main.run main
