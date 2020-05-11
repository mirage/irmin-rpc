open Lwt.Infix
open Irmin_rpc.Private.Utils
module Server = Irmin_mem.KV (Irmin.Contents.String)
module RPC = Irmin_rpc.Make (Server) (Irmin_rpc.Codec.Unit)

(** API changes to ease test-writing. Might want to upstream these at some
    point. *)
module Client = struct
  include RPC.Client.Store

  let of_branch = Fun.flip of_branch
end

(** Extended [TESTABLE]s for store types. *)
module Alcotest = struct
  let of_typ (type a) (t : a Irmin.Type.t) : a Alcotest.testable =
    Alcotest.testable (Irmin.Type.pp t) (Irmin.Type.equal t)

  let tree = of_typ Server.Tree.concrete_t

  let info = of_typ Irmin.Info.t

  let find = Alcotest.(result (option string) reject)

  let find_tree = Alcotest.(option tree)

  include Alcotest
end

(** Helpers for constructing data. *)

module Faker = struct
  let () = Random.self_init ()

  let string ?(length = 10) () =
    String.init length (fun _i -> Random.int 256 |> Char.chr)

  let info () =
    let date = Random.int64 Int64.max_int
    and author = string ()
    and msg = string () in
    Irmin.Info.v ~date ~author msg
end

(** Tree with a single child *)
let stree only_key only_child = `Tree [ (only_key, only_child) ]

(** Sequence of nested trees each with exactly one child *)
let strees : string list -> Server.Tree.concrete -> Server.Tree.concrete =
  List.fold_right stree

let contents v = `Contents (v, ())

let ( let+ ) x f = Lwt.map f x

let ( let* ) = Lwt.bind

type ctx = { client : Client.repo; server : Server.repo }
(** Each test gets its own client/server pair over a fresh in-memory Irmin
    store. *)

let ctx () =
  let+ server = Server.Repo.v (Irmin_mem.config ()) in
  let client = RPC.Server.Repo.local server in
  { server; client }

(** Default info. *)
let info = Irmin.Info.none

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
  Client.get master [ "k" ] >|= Alcotest.(check string) "Binding [k → v]" "v"

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
        [ ("leaf", contents "data1"); ("branch", stree "f" (contents "data2")) ])
  in
  let* () =
    let* master = server |> Server.master in
    tree |> Server.Tree.of_concrete |> Server.set_tree_exn master ~info [ "k" ]
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

let () =
  let test_case name fn =
    Alcotest_lwt.test_case name `Quick (fun _switch () -> ctx () >>= fn)
  in
  Alcotest_lwt.run "irmin-rpc"
    [
      ( "irmin-rpc",
        [
          test_case "master" test_master;
          test_case "of_branch" test_of_branch;
          test_case "get" test_get;
          test_case "find" test_find;
          test_case "find_tree" test_find_tree;
          test_case "set" test_set;
        ] );
    ]
  |> Lwt_main.run
