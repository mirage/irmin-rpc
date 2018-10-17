open Lwt.Infix

module Store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Rpc = Irmin_rpc_unix.Make(Store)(struct
  let remote = Store.remote
end)

(* This was printed when running the server example *)
let uri = "capnp://sha-256:HUOdhEKv0Knk5USkfaFiXCC_l_s3dYjoayyrmu_olh4@127.0.0.1:9999/yxEIPPXH-w8pTd_ULcm4AmUsZwA5QrSfSZj_z_Vzulw"

let main =
    Rpc.Client.connect (Uri.of_string uri) >>= fun client ->
    Rpc.Client.set client ["abc"] "123" >>= fun _ ->
    Rpc.Client.get client ["abc"] >|= fun res ->
    assert (res = "123"); print_endline res

let () = Lwt_main.run main
