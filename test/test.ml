open Lwt.Infix
open Common
open Irmin_rpc.Private.Utils
module Server = Irmin_mem.KV (Irmin.Contents.String)
module RPC = Irmin_rpc.Make (Server) (Irmin_rpc.Codec.Unit)

(** API changes to ease test-writing. Might want to upstream these at some
    point. *)
module Client = struct
  include RPC.Client.Store

  let of_branch = Fun.flip of_branch
end

let ( let+ ) x f = Lwt.map f x

let ( let* ) = Lwt.bind

(** Default info. *)
let info = Irmin.Info.none

module Test_store = struct
  type ctx = { client : Client.repo; server : Server.repo }
  (** Each test gets its own client/server pair over a fresh in-memory Irmin
      store. *)

  let ctx () =
    let+ server = Server.Repo.v (Irmin_mem.config ()) in
    let client = RPC.Server.Repo.local server in
    { server; client }

  let test_case name (fn : ctx -> unit Lwt.t) =
    Alcotest_lwt.test_case name `Quick (fun _switch () -> ctx () >>= fn)

  (** Tests *)

  let test_master { client; _ } =
    let+ (_ : Client.t) = client |> Client.master in
    ()

  let test_of_branch { client; _ } =
    let+ (_ : Client.t) = client |> Client.of_branch "foo" in
    ()

  let test_get { server; client } =
    let* () =
      let* master = server |> Server.master in
      Server.set_exn master ~info [ "k" ] "v"
    in
    let* master = client |> Client.master in
    Client.get master [ "k" ]
    >|= Alcotest.(check string) "Binding [k → v]" "v"

  let test_find { server; client } =
    let* () =
      let* master = server |> Server.master in
      Server.set_exn master ~info [ "k" ] "v"
    in
    let* master = client |> Client.master in
    let* () =
      Client.find master [ "k" ]
      >|= Alcotest.(check find) "Binding [k → Some v]" (Ok (Some "v"))
    in
    let* () =
      Client.find master [ "k_absent" ]
      >|= Alcotest.(check find) "Binding [k_absent → None]" (Ok None)
    in
    Lwt.return ()

  let test_find_tree { server; client } =
    let tree =
      strees [ "a"; "b"; "c" ]
        (`Tree
          [
            ("leaf", contents "data1"); ("branch", stree "f" (contents "data2"));
          ])
    in
    let* () =
      let* master = server |> Server.master in
      tree
      |> Server.Tree.of_concrete
      |> Server.set_tree_exn master ~info [ "k" ]
    in
    let* master = client |> Client.master in
    let* () =
      Client.find_tree master [ "k" ]
      >>= Option.map_lwt Server.Tree.to_concrete
      >|= Alcotest.(check find_tree) "Binding [k → Some tree]" (Some tree)
    in
    let* () =
      Client.find_tree master [ "k_absent" ]
      >>= Option.map_lwt Server.Tree.to_concrete
      >|= Alcotest.(check find_tree) "Binding [k_absent → Some tree]" None
    in
    Lwt.return ()

  let test_set { server; client } =
    let info = Faker.info () in
    let* () =
      let* master = client |> Client.master in
      Client.set ~info:(fun () -> info) master [ "k" ] "v"
    in
    let* master = server |> Server.master in
    let* () =
      Server.get master [ "k" ]
      >|= Alcotest.(check string) "Binding [k → v]" "v"
    in
    let* () =
      Server.Head.get master
      >|= Server.Commit.info
      >|= Alcotest.(check info) "New commit has the correct info" info
    in
    Lwt.return ()

  let suite =
    [
      test_case "master" test_master;
      test_case "of_branch" test_of_branch;
      test_case "get" test_get;
      test_case "find" test_find;
      test_case "find_tree" test_find_tree;
      test_case "set" test_set;
    ]
end

let () =
  Alcotest_lwt.run "irmin-rpc"
    [
      ("utils", Test_utils.suite);
      ("store", Test_store.suite);
      ("disconnected", Test_disconnected.suite);
    ]
  |> Lwt_main.run
