open Lwt.Infix
module Underlying_store = Irmin_mem.KV (Irmin.Contents.String)
module RPC = Irmin_rpc.Make (Underlying_store) (Irmin_rpc.Codec.Unit)
module Store = RPC.Client.Store

let check_repo, spawn_server =
  let repo = Underlying_store.Repo.v (Irmin_mem.config ()) in
  let check_repo : string -> (Underlying_store.repo -> bool Lwt.t) -> unit Lwt.t
      =
   fun msg pred ->
    repo >>= pred >|= function true -> () | false -> Alcotest.fail msg
  and spawn_server : unit -> Store.repo Lwt.t =
   fun () -> repo >|= RPC.Server.Repo.local
  in
  (check_repo, spawn_server)

let () = Alcotest_lwt.run "irmin-rpc" [] |> Lwt_main.run
