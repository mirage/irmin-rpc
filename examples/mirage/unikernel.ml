open Lwt.Infix
module Store = Irmin_mem.KV (Irmin.Contents.String)

module Main
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock_lwt.MCLOCK)
    (Pclock : Mirage_clock_lwt.PCLOCK)
    (Time : Mirage_time_lwt.S)
    (Stack : Mirage_stack_lwt.V4) =
struct
  module Rpc =
    Irmin_rpc_mirage.Make (Store) (Irmin_rpc.Codec.Unit) (Random) (Mclock)
      (Pclock)
      (Time)
      (Stack)

  let start _random _mclock _pclock _time stack =
    let port = 8888 in
    let ipv4 = Stack.ipv4 stack in
    let addr = Stack.IPV4.get_ip ipv4 |> List.hd in
    let addr = Ipaddr.V4.to_string addr in
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
    Logs.info (fun f -> f "Running server: %s:%d" addr port);
    Rpc.Server.serve ~secret_key:`Ephemeral stack repo ~port ~addr
end
