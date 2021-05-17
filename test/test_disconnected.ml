open Irmin_rpc.Private.Utils
open Lwt.Infix
open Common
module S_server = Irmin_mem.KV.Make (Irmin.Contents.String)
module RPC =
  Irmin_rpc.Make
    (Server)
    (Irmin_rpc.Config.Remote.None (S_server))
    (Irmin_rpc.Config.Pack.None (Server))
module Client = RPC.Client
module S_client = RPC.Client.Store

(** Tests of the behaviour of the Irmin_rpc API when various capabilities are
    broken. *)

let ( let* ) = Lwt.bind

type ctx = {
  irmin : RPC.Client.t;
  store : S_client.t;
  repo : Client.repo;
  commit : Client.commit;
}
(** Each test consumes a broken capability of each type. *)

let broken_exception = Capnp_rpc.Exception.v "Broken test capability"

let ctx () =
  let b () = Capnp_rpc_lwt.Capability.broken broken_exception in
  { irmin = b (); store = b (); repo = b (); commit = b () }

let check_failure (type a) (fn : unit -> a Lwt.t) : unit Lwt.t =
  Lwt.catch
    (fun () ->
      fn () >>= fun _ -> Alcotest.fail "Expected thread failure did not occur")
    (function
      | Failure error_msg -> (
          (* Capnp error messages are too brittle to rely upon in test, so we
             just require that the message contains the string built into the
             broken capability above. *)
          match String.is_substring "Broken test capability" error_msg with
          | true -> Lwt.return_unit
          | false ->
              Alcotest.failf "Unexpected Failure error message: %s" error_msg)
      | exn -> Lwt.fail exn)

let test_case name (fn : ctx -> unit Lwt.t) =
  Alcotest_lwt.test_case name `Quick (fun _switch () -> ctx () |> fn)

(** Test cases *)

let test_ping { irmin; _ } =
  let* () =
    RPC.Client.ping irmin
    >|= Alcotest.(check (result reject capnp_error))
          "Error case"
          (Error (`Capnp (`Exception broken_exception)))
  in
  Lwt.return_unit

let test_master { repo; _ } =
  (* [master] is pipelined, so we don't observe a failure until we _use_ the
     store capability. *)
  let* s = S_client.master repo in
  let* () = check_failure (fun () -> S_client.get s []) in
  Lwt.return_unit

let test_of_branch { repo; _ } =
  let* s = S_client.of_branch repo "foo" in
  let* () = check_failure (fun () -> S_client.get s []) in
  Lwt.return_unit

let suite =
  [
    test_case "Irmin.ping" test_ping;
    test_case "Store.master" test_master;
    test_case "Store.of_branch" test_of_branch;
  ]
