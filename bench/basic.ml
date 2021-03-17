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

let rec add tree n =
  if n = 0 then Lwt.return_unit
  else
    let s = random_string 512 in
    let key = [ string_of_int n ] in
    let* () = Rpc.Client.Tx.add tree key s in
    add tree (n - 1)

let rpc url n =
  let* n, () =
    let* client = Rpc.Client.connect (Uri.of_string url) in
    let* repo = Rpc.Client.repo client in
    let* master = Rpc.Client.Store.master repo in
    let* tx = Rpc.Client.Tx.empty repo in
    with_timer (fun () ->
        let* () = add tx n in
        let* () =
          Rpc.Client.Tx.commit tx master [ "a" ] ~info:(Irmin_unix.info "test")
        in
        Lwt.return_unit)
  in
  Lwt_io.printf "%f\n" n

let () =
  let n =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10_000
  in
  Memtrace.trace_if_requested ();
  Lwt_main.run (rpc Sys.argv.(1) n)
