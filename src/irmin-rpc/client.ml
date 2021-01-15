include Client_intf
open Capnp_rpc_lwt
open Lwt.Infix
open Utils

module type S = S

module type MAKER = MAKER

exception Error_message of string

exception Remote_error of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

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
        Capability.call_for_caps t method_id req Results.store_get_pipelined
        |> Lwt.return

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
        Params.key_set p (Codec.Key.encode key);
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

      let sync t =
        let open Raw.Client.Store.Sync in
        let req = Capability.Request.create_no_args () in
        Capability.call_for_caps t method_id req Results.sync_get_pipelined
        |> Lwt.return

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
          Params.commit_set p (Some commit);
          Capability.call_for_value_exn t method_id req >|= fun _res -> ()
      end

      module Commit = struct
        let of_hash repo hash : commit option Lwt.t =
          let open Raw.Client.Repo.CommitOfHash in
          let req, p = Capability.Request.create Params.init_pointer in
          Params.hash_set p (Codec.Hash.encode hash);
          Capability.call_for_value_exn repo method_id req
          >|= Results.commit_get

        let hash commit =
          let open Raw.Client.Commit.Hash in
          let req = Capability.Request.create_no_args () in
          Capability.call_for_value_exn commit method_id req >|= fun res ->
          Results.hash_get res |> Codec.Hash.decode |> Result.get_ok

        let info commit =
          let open Raw.Client.Commit.Info in
          let req = Capability.Request.create_no_args () in
          Capability.call_for_value_exn commit method_id req >|= fun res ->
          Results.info_get res |> Codec.Info.decode

        let tree commit =
          let open Raw.Client.Commit.Tree in
          let req = Capability.Request.create_no_args () in
          Capability.call_for_value_exn commit method_id req >|= fun res ->
          Results.tree_get res |> Codec.Tree.decode

        let parents commit =
          let open Raw.Client.Commit.Parents in
          let req = Capability.Request.create_no_args () in
          Capability.call_for_value_exn commit method_id req >|= fun res ->
          Results.hashes_get_list res
          |> List.map (fun x -> Codec.Hash.decode x |> Result.get_ok)
      end

      module Sync = struct
        type endpoint = Endpoint_codec.t

        let clone t endpoint =
          let open Raw.Client.Sync.Clone in
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Endpoint_codec.encode endpoint);
          Capability.call_for_caps t method_id req Results.result_get_pipelined
          |> Lwt.return

        let pull t ~info endpoint =
          let open Raw.Client.Sync.Pull in
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Endpoint_codec.encode endpoint);
          let _ = Params.info_set_builder p (Codec.Info.encode @@ info ()) in
          Capability.call_for_caps t method_id req Results.result_get_pipelined
          |> Lwt.return

        let decode_push_result t =
          let open Raw.Reader.Sync.PushResult in
          match get t with
          | OkEmpty -> Lwt.return @@ Ok `Empty
          | OkHead head ->
              let hash : hash = Codec.Hash.decode head |> unwrap in
              Lwt.return @@ Ok (`Head hash)
          | ErrorDetachedHead -> Lwt.return @@ Error `Detached_head
          | ErrorMsg msg -> Lwt.return @@ Error (`Msg msg)
          | Undefined _ -> Lwt.return @@ Error (`Msg "Undefined")

        let push t endpoint =
          let open Raw.Client.Sync.Push in
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Endpoint_codec.encode endpoint);
          let* (x : Results.t) =
            Capability.call_for_value_exn t method_id req
          in
          let x = Results.result_get x in
          decode_push_result x
      end
    end

    let repo t =
      let open Raw.Client.Irmin.Repo in
      let req = Capability.Request.create_no_args () in
      Capability.call_for_caps t method_id req Results.repo_get_pipelined
      |> Lwt.return

    let ping t =
      let open Raw.Client.Irmin.Ping in
      let req = Capability.Request.create_no_args () in
      Capability.call_for_value t method_id req >|= Result.map (fun _ -> ())
  end
