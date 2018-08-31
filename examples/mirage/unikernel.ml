open Irmin_rpc_mirage
open Lwt.Infix

module Store = Irmin_mem.KV(Irmin.Contents.String)

module Main (Clock: Mirage_clock_lwt.PCLOCK) (Time: Mirage_time_lwt.S) (Stack: Mirage_stack_lwt.V4) = struct
  module Rpc = Irmin_rpc_mirage.Make(Store)(Clock)(Time)(Stack)

  let start _clock _time _stack _nocrypto =
    let module Server = Rpc.Server(struct
      let clock = _clock
    end) in
    let port = 8888 in
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
    Logs.info (fun f -> f "STARTING SERVER");
    Server.create ~secret_key:`Ephemeral _stack repo ~port ~addr:"0.0.0.0" >>= fun server ->
    Logs.info (fun f -> f "%s" (Uri.to_string @@ Server.uri server));
    Server.run server
end
