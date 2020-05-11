open Lwt.Infix
module Underlying_store = Irmin_unix.Git.Mem.KV (Irmin.Contents.String)
module Rpc =
  Irmin_rpc_unix.Make
    (Underlying_store)
    (Irmin_rpc_unix.Git_unix_endpoint_codec)
module Store = Rpc.Client.Store

(* This was printed when running the server example *)
let uri =
  "capnp://sha-256:HUOdhEKv0Knk5USkfaFiXCC_l_s3dYjoayyrmu_olh4@127.0.0.1:9999/yxEIPPXH-w8pTd_ULcm4AmUsZwA5QrSfSZj_z_Vzulw"

let info () =
  Irmin.Info.v ~date:0L ~author:"rpc-client-author" "rpc-client-message"

let ( let* ) = Lwt.bind

let main =
  let* repo = Uri.of_string uri |> Rpc.Client.connect >>= Rpc.Client.repo in
  let* master = Store.master repo in
  let* () = Store.set ~info master [ "abc" ] "123" in
  let* res = Store.get master [ "abc" ] in
  assert (res = "123");
  print_endline res;
  Lwt.return ()

let () = Lwt_main.run main
