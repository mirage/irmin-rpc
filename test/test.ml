open Lwt.Infix
open Lwt.Syntax
open Common
open Irmin_rpc.Private.Utils
module Server = Irmin_mem.KV (Irmin.Contents.String)
module RPC =
  Irmin_rpc.Make
    (Server)
    (Irmin_rpc.Config.Remote.None (Server))
    (Irmin_rpc.Config.Pack.None (Server))

(** API changes to ease test-writing. Might want to upstream these at some
    point. *)
module Client = struct
  include RPC.Client

  let of_branch = Fun.flip Store.of_branch
end

(** Default info. *)
let info = Irmin.Info.none

open Client

module Test_store = struct
  type ctx = { client : Client.repo; server : Server.repo }
  (** Each test gets its own client/server pair over a fresh in-memory Irmin
      store. *)

  let ctx () =
    let+ server = Server.Repo.v (Irmin_mem.config ()) in
    let client = RPC.Server.Repo.local server in
    { server; client }

  let rec resolve_tree server (x : Client.Tree.concrete) =
    match x with
    | `Contents x ->
        let+ c = Server.Contents.of_hash server x in
        let c = Option.get c in
        `Contents (c, Server.Metadata.default)
    | `Tree l ->
        let+ l =
          Lwt_list.map_s
            (fun (step, t) ->
              let+ t = resolve_tree server t in
              (step, t))
            l
        in
        `Tree l

  let test_case name (fn : ctx -> unit Lwt.t) =
    Alcotest_lwt.test_case name `Quick (fun _switch () -> ctx () >>= fn)

  (** Tests *)

  let test_master { client; _ } =
    let+ (_ : Store.t) = client |> Store.master in
    ()

  let test_of_branch { client; _ } =
    let+ (_ : Store.t) = client |> Client.of_branch "foo" in
    ()

  let test_get { server; client } =
    let* () =
      let* master = server |> Server.master in
      Server.set_exn master ~info [ "k" ] "v"
    in
    let* master = client |> Store.master in
    Store.get master [ "k" ] >|= Alcotest.(check string) "Binding [k → v]" "v"

  let test_find { server; client } =
    let* () =
      let* master = server |> Server.master in
      Server.set_exn master ~info [ "k" ] "v"
    in
    let* master = client |> Store.master in
    let* () =
      Store.find master [ "k" ]
      >|= Alcotest.(check find) "Binding [k → Some v]" (Ok (Some "v"))
    in
    let* () =
      Store.find master [ "k_absent" ]
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
    let* master = client |> Store.master in
    let* () =
      Client.Store.find_tree master [ "k" ]
      >>= Option.map_lwt Client.Tree.concrete
      >>= Option.map_lwt (resolve_tree server)
      >|= Alcotest.(check find_tree) "Binding [k → Some tree]" (Some tree)
    in
    let* () =
      Client.Store.find_tree master [ "k_absent" ]
      >>= Option.map_lwt Client.Tree.concrete
      >>= Option.map_lwt (resolve_tree server)
      >|= Alcotest.(check find_tree) "Binding [k_absent → Some tree]" None
    in
    Lwt.return ()

  let test_set { server; client } =
    let info = Faker.info () in
    let* () =
      let* master = client |> Store.master in
      Store.set ~info:(fun () -> info) master [ "k" ] "v"
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

  let random_string n =
    String.init n (fun _ -> char_of_int (31 + Random.int 95))

  let test_tree { server; client } =
    let info () = Faker.info () in
    let* master = Client.Store.master client in
    let* tree = Client.Tree.empty client in
    let* tx = Client.Tx.v client tree in
    let* () = Client.Tx.add tx [ "a" ] (random_string 2048) in
    let* () = Client.Tx.add_tree tx [ "b" ] tree in
    let* () = Client.Tx.remove tx [ "b" ] in
    let* tree = Client.Tx.tree tx in
    let* () = Client.Store.set_tree master [ "tree" ] tree ~info in
    let* tree = Client.Tree.concrete tree >>= resolve_tree server in
    let* master = Server.master server in
    let* () =
      Server.find_tree master [ "tree" ]
      >>= Option.map_lwt Server.Tree.to_concrete
      >|= Alcotest.(check find_tree) "tree matches" (Some tree)
    in
    Lwt.return ()

  let test_test_and_set { server; client } =
    let info () = Faker.info () in
    let* master = Client.Store.master client in
    let s = random_string 1024 in
    let s' = random_string 1024 in
    let* ok =
      Client.Store.test_and_set master ~info [ "test"; "set" ] ~test:None
        ~set:(Some s)
    in
    Alcotest.(check bool) "test and set, initial value" true ok;
    let* ok =
      Client.Store.test_and_set master ~info [ "test"; "set" ] ~test:None
        ~set:(Some s')
    in
    Alcotest.(check bool) "test and set, incorrect value" false ok;
    let* ok =
      Client.Store.test_and_set master ~info [ "test"; "set" ] ~test:(Some s)
        ~set:(Some s')
    in
    Alcotest.(check bool) "test and set, correct value" true ok;
    let* master = Server.master server in
    let* v = Server.find master [ "test"; "set" ] in
    Alcotest.(check (option string))
      "test and set, value from store" (Some s') v;
    Lwt.return ()

  let test_test_and_set_tree { server; client } =
    let info () = Faker.info () in
    let* master = Client.Store.master client in
    let* tree = Client.Tree.empty client in
    let* tx = Client.Tx.v client tree in
    let* () = Client.Tx.add tx [ "a" ] "1" in
    let* () = Client.Tx.add tx [ "b" ] "2" in
    let* () = Client.Tx.add tx [ "c" ] "3" in
    let* tree = Client.Tx.tree tx in
    let* ok =
      Client.Store.test_and_set_tree master ~info [ "test"; "tree" ] ~test:None
        ~set:(Some tree)
    in
    Alcotest.(check bool) "test and set tree, initial value" true ok;
    let* ok =
      Client.Store.test_and_set_tree master ~info [ "test"; "tree" ] ~test:None
        ~set:None
    in
    Alcotest.(check bool) "test and set tree, incorrect value" false ok;
    let* ok =
      Client.Store.test_and_set_tree master ~info [ "test"; "tree" ]
        ~test:(Some tree) ~set:None
    in
    Alcotest.(check bool) "test and set tree, correct value" true ok;
    let* master = Server.master server in
    let* v = Server.find master [ "test"; "tree" ] in
    Alcotest.(check (option string))
      "test and set tree, value from store" None v;
    Lwt.return ()

  let suite =
    [
      test_case "master" test_master;
      test_case "of_branch" test_of_branch;
      test_case "get" test_get;
      test_case "find" test_find;
      test_case "find_tree" test_find_tree;
      test_case "set" test_set;
      test_case "tree" test_tree;
      test_case "test_and_set" test_test_and_set;
      test_case "test_and_set_tree" test_test_and_set_tree;
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
