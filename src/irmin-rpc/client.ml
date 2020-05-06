include Client_intf
open Capnp_rpc_lwt
open Lwt.Infix

module type S = S

module type MAKER = MAKER

exception Error_message of string

exception Remote_error of string

[@@@warning "-32"]

[@@@warning "-33"]

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

let error (`Capnp err) =
  let s = Fmt.to_to_string Capnp_rpc.Error.pp err in
  Error (`Msg s)

let todo = assert false

module Make : MAKER =
functor
  (Store : Irmin.S)
  (Endpoint_codec : Codec.SERIALISABLE with type t = Store.Private.Sync.endpoint)
  ->
  struct
    module Codec = Codec.Make (Store)

    module Store = struct
      include Types

      type tree = Store.tree

      type branch = Store.branch

      type key = Store.key

      type contents = Store.contents

      type hash = Store.hash

      let find t key =
        let open Raw.Client.Store.Find in
        let ( >>| ) x f = Result.map f x in
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        match Results.has_result res with
        | true ->
            Results.result_get res |> Codec.Contents.decode >>| Option.some
        | false -> Ok None

      let get t key =
        find t key >|= function
        | Ok (Some c) -> c
        | Ok None -> invalid_arg "Irmin_rpc: no blob found during get"
        | Error (`Msg m) -> raise (Remote_error m)

      let find_tree t key =
        let open Raw.Client.Store.FindTree in
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        match Results.has_result res with
        | true ->
            Results.result_get res
            |> Codec.Tree.decode
            |> Store.Tree.of_concrete
            |> Option.some
        | false -> None

      let set = todo

      let set_tree = todo

      let remove = todo

      let merge_into = todo

      module Branch = struct
        let list = todo

        let remove = todo

        let set = todo
      end
    end

    let heartbeat = todo
  end
