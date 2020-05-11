include Client_intf
open Capnp_rpc_lwt
open Lwt.Infix
open Utils

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

let ( let* ) = Lwt.bind

module Make : MAKER =
functor
  (Store : Irmin.S)
  (Endpoint_codec : Codec.SERIALISABLE with type t = Store.Private.Sync.endpoint)
  ->
  struct
    module Codec = Codec.Make (Store)

    type t = Raw.Client.Irmin.t Capability.t

    module Store = struct
      include Types

      type tree = Store.tree

      type branch = Store.branch

      type key = Store.key

      type contents = Store.contents

      type hash = Store.hash

      let master t =
        let open Raw.Client.Repo.Master in
        let req = Capability.Request.create_no_args () in
        Capability.call_for_caps t method_id req Results.store_get_pipelined
        |> Lwt.return

      let of_branch t branch =
        let open Raw.Client.Repo.OfBranch in
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        Capability.call_for_value_exn t method_id req
        >|= (Results.store_get >> Option.get)

      let find t key =
        let open Raw.Client.Store.Find in
        let ( >>| ) x f = Result.map f x in
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        match Results.has_contents res with
        | true ->
            Results.contents_get res |> Codec.Contents.decode >>| Option.some
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
        match Results.has_tree res with
        | true -> Results.tree_get res |> Codec.Tree.decode |> Option.some
        | false -> None

      let set ~info t key contents =
        let open Raw.Client.Store.Set in
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Codec.Contents.encode contents |> Params.contents_set p;
        Capability.call_for_value_exn t method_id req >|= fun _res -> ()

      let set_tree ~info t key tree =
        let open Raw.Client.Store.SetTree in
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        let* (_ : Raw.Builder.Tree.t) =
          tree |> Codec.Tree.encode >|= Params.tree_set_builder p
        in
        Capability.call_for_value_exn t method_id req >|= fun _res -> ()

      let remove ~info t key =
        let open Raw.Client.Store.Remove in
        let req, p = Capability.Request.create Params.init_pointer in
        key |> Codec.Key.encode |> Params.key_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Capability.call_for_value_exn t method_id req >|= fun _res -> ()

      let merge_with_branch t ~info branch =
        let open Raw.Client.Store.MergeWithBranch in
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Capability.call_for_value_exn t method_id req
        >|= (Results.result_get >> Codec.Merge_result.decode >> unwrap)

      module Branch = struct
        let list t =
          let open Raw.Client.Repo.BranchList in
          let req = Capability.Request.create_no_args () in
          Capability.call_for_value_exn t method_id req
          >|= ( Results.branches_get_list
              >> List.map (Codec.Branch.decode >> unwrap) )

        let remove t branch =
          let open Raw.Client.Repo.BranchRemove in
          let req, p = Capability.Request.create Params.init_pointer in
          branch |> Codec.Branch.encode |> Params.branch_set p;
          Capability.call_for_value_exn t method_id req >|= fun _res -> ()

        let set t branch commit =
          let open Raw.Client.Repo.BranchSet in
          let req, p = Capability.Request.create Params.init_pointer in
          branch |> Codec.Branch.encode |> Params.branch_set p;
          Some commit |> Params.commit_set p;
          Capability.call_for_value_exn t method_id req >|= fun _res -> ()
      end
    end

    let repo t =
      let open Raw.Client.Irmin.Repo in
      let req = Capability.Request.create_no_args () in
      Capability.call_for_value_exn t method_id req
      >|= (Results.repo_get >> Option.get)

    let heartbeat t msg =
      let open Raw.Client.Irmin.Heartbeat in
      let req, p = Capability.Request.create Params.init_pointer in
      Params.msg_set p msg;
      Capability.call_for_value_exn t method_id req >|= Results.reply_get
  end
