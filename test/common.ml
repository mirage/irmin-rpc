module Server = Irmin_mem.KV (Irmin.Contents.String)
module RPC =
  Irmin_rpc.Make
    (Server)
    (Irmin_rpc.Config.Remote.None (Server))
    (Irmin_rpc.Config.Pack.None (Server))

(** API changes to ease test-writing. Might want to upstream these at some
    point. *)
module Client = struct
  include RPC.Client.Store

  let of_branch = Fun.flip of_branch
end

(** Extended [TESTABLE]s for store types. *)
module Alcotest = struct
  let of_typ (type a) (t : a Irmin.Type.t) : a Alcotest.testable =
    Alcotest.testable (Irmin.Type.pp t)
      (Irmin.Type.unstage @@ Irmin.Type.equal t)

  let msg : [ `Msg of string ] Alcotest.testable =
    Alcotest.testable
      (fun ppf (`Msg msg) -> Fmt.pf ppf "Msg %s" msg)
      (fun (`Msg a) (`Msg b) -> String.equal a b)

  (* TODO: upstream these equality functions to Capnp_rpc *)

  let capnp_exception_equal (a : Capnp_rpc.Exception.t)
      (b : Capnp_rpc.Exception.t) =
    String.equal a.reason b.reason
    &&
    match (a.ty, b.ty) with
    | `Disconnected, `Disconnected
    | `Failed, `Failed
    | `Overloaded, `Overloaded
    | `Unimplemented, `Unimplemented ->
        true
    | `Undefined a, `Undefined b -> Int.equal a b
    | (`Disconnected | `Failed | `Overloaded | `Unimplemented | `Undefined _), _
      ->
        false

  let capnp_error_equal (a : Capnp_rpc.Error.t) (b : Capnp_rpc.Error.t) =
    match (a, b) with
    | `Cancelled, `Cancelled -> true
    | `Exception a, `Exception b -> capnp_exception_equal a b
    | (`Cancelled | `Exception _), _ -> false

  let capnp_exception : Capnp_rpc.Exception.t Alcotest.testable =
    Alcotest.testable Capnp_rpc.Exception.pp capnp_exception_equal

  let capnp_error : [ `Capnp of Capnp_rpc.Error.t ] Alcotest.testable =
    Alcotest.testable
      (fun ppf (`Capnp err) -> Fmt.pf ppf "Capnp %a" Capnp_rpc.Error.pp err)
      (fun (`Capnp a) (`Capnp b) -> capnp_error_equal a b)

  let tree = of_typ Server.Tree.concrete_t

  let info = of_typ Irmin.Info.t

  let find = Alcotest.(result (option string) msg)

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

(** Tree with a single child. *)
let stree only_key only_child = `Tree [ (only_key, only_child) ]

(** Sequence of nested trees each with exactly one child. *)
let strees : string list -> Server.Tree.concrete -> Server.Tree.concrete =
  List.fold_right stree

let contents v = `Contents (v, ())
