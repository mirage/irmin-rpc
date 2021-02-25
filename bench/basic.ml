open Bench_common
open Lwt.Syntax
module Store =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)
module Rpc =
  Irmin_rpc_unix.Make
    (Store)
    (Irmin_rpc.Config.Remote.None (Store))
    (Irmin_rpc.Config.Pack.Make (Store))

let rec rpc_add tx n =
  if n = 0 then Lwt.return tx
  else
    let s = random_string (Random.int 1024) in
    let key = [ random_key () ] in
    let* tx = Rpc.Client.Tree.Local.add tx key s in
    rpc_add tx (n - 1)

let rpc url =
  let* n, () =
    with_timer (fun () ->
        let* client = Rpc.Client.connect (Uri.of_string url) in
        let* repo = Rpc.Client.repo client in
        let* master = Rpc.Client.Store.master repo in
        let tx = Rpc.Client.Tree.Local.empty in
        let* tx = rpc_add tx 100000 in
        let* tree = Rpc.Client.Tree.Local.to_tree repo tx in
        let* () =
          Rpc.Client.Store.set_tree master ~info:(Irmin_unix.info "test") []
            tree
        in
        Lwt.return_unit)
  in
  Lwt_io.printf "%f\n" n

let () = Lwt_main.run (rpc Sys.argv.(1))
