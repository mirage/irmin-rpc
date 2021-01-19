include Client_intf
open Capnp_rpc_lwt
open Utils
open Lwt.Syntax

module type S = S

module type MAKER = MAKER

exception Error_message of string

exception Remote_error of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

let ( let* ) = Lwt.bind

module Make : MAKER =
functor
  (Store : Irmin.S)
  (Remote : Config.REMOTE with type t = Store.Private.Sync.endpoint)
  (Pack : Config.PACK with type repo = Store.repo)
  ->
  struct
    module Codec = Codec.Make (Store)

    type t = Raw.Client.Irmin.t Capability.t

    let remote =
      match Remote.v with
      | Some x -> x
      | None ->
          ( module struct
            type t = Store.Private.Sync.endpoint

            let fail () = failwith "SYNC API is unimplemented"

            let decode _ = fail ()

            let encode _ = fail ()
          end )

    module Store = struct
      include Types

      type tree = Store.tree

      type branch = Store.branch

      type key = Store.key

      type contents = Store.contents

      type hash = Store.hash

      let master t =
        let open Raw.Client.Repo.Master in
        Logs.info (fun l -> l "Store.master");
        let req = Capability.Request.create_no_args () in
        Capability.call_for_caps t method_id req Results.store_get_pipelined
        |> Lwt.return

      let of_branch t branch =
        let open Raw.Client.Repo.OfBranch in
        Logs.info (fun l ->
            l "Store.of_branch: %a" (Irmin.Type.pp Store.Branch.t) branch);
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        Capability.call_for_caps t method_id req Results.store_get_pipelined
        |> Lwt.return

      let find t key =
        let open Raw.Client.Store.Find in
        log_key (module Store) "Store.find" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let+ res = Capability.call_for_value_exn t method_id req in
        match Results.has_contents res with
        | true ->
            Result.map Option.some
              (Results.contents_get res |> Codec.Contents.decode)
        | false -> Ok None

      let get t key =
        let+ value = find t key in
        match value with
        | Ok (Some c) -> c
        | Ok None -> invalid_arg "Irmin_rpc: no blob found during get"
        | Error (`Msg m) -> raise (Remote_error m)

      let find_tree t key =
        let open Raw.Client.Store.FindTree in
        log_key (module Store) "Store.find_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let+ res = Capability.call_for_value_exn t method_id req in
        match Results.has_tree res with
        | true -> Results.tree_get res |> Codec.Tree.decode |> Option.some
        | false -> None

      let set ~info t key contents =
        let open Raw.Client.Store.Set in
        log_key (module Store) "Store.set" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Codec.Contents.encode contents |> Params.contents_set p;
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()

      let set_tree ~info t key tree =
        let open Raw.Client.Store.SetTree in
        log_key (module Store) "Store.set_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        let* e = Codec.Tree.encode tree in
        let (_ : Raw.Builder.Tree.t) = Params.tree_set_builder p e in
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()

      let remove ~info t key =
        let open Raw.Client.Store.Remove in
        log_key (module Store) "Store.remove" key;
        let req, p = Capability.Request.create Params.init_pointer in
        key |> Codec.Key.encode |> Params.key_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()

      let merge_with_branch t ~info branch =
        let open Raw.Client.Store.MergeWithBranch in
        Logs.info (fun l ->
            l "Store.merge_with_branch: %a"
              (Irmin.Type.pp Store.Branch.t)
              branch);
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        let+ res = Capability.call_for_value_exn t method_id req in
        Results.result_get res |> Codec.Merge_result.decode |> unwrap

      let sync t =
        if Option.is_none Remote.v then Lwt.return None
        else
          let open Raw.Client.Store.Sync in
          Logs.info (fun l -> l "Store.sync");
          let req = Capability.Request.create_no_args () in
          Lwt.return
          @@ Some
               (Capability.call_for_caps t method_id req
                  Results.sync_get_pipelined)

      let pack t =
        if Option.is_some Pack.v then Lwt.return None
        else
          let open Raw.Client.Store.Pack in
          Logs.info (fun l -> l "Store.pack");
          let req = Capability.Request.create_no_args () in
          let cap =
            Capability.call_for_caps t method_id req Results.pack_get_pipelined
          in
          Lwt.return @@ Some cap

      let last_modified t key =
        let open Raw.Client.Store.LastModified in
        log_key (module Store) "Store.last_modified" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        Lwt.return
        @@ Capability.call_for_caps t method_id req Results.commit_get_pipelined

      module Branch = struct
        let list t =
          let open Raw.Client.Repo.BranchList in
          Logs.info (fun l -> l "Branch.list");
          let req = Capability.Request.create_no_args () in
          let+ res = Capability.call_for_value_exn t method_id req in
          Results.branches_get_list res
          |> List.map (Codec.Branch.decode >> unwrap)

        let remove t branch =
          let open Raw.Client.Repo.BranchRemove in
          Logs.info (fun l ->
              l "Branch.remove: %a" (Irmin.Type.pp Store.Branch.t) branch);
          let req, p = Capability.Request.create Params.init_pointer in
          branch |> Codec.Branch.encode |> Params.branch_set p;
          let+ _ = Capability.call_for_value_exn t method_id req in
          ()

        let set t branch commit =
          let open Raw.Client.Repo.BranchSet in
          Logs.info (fun l ->
              l "Branch.set: %a" (Irmin.Type.pp Store.Branch.t) branch);
          let req, p = Capability.Request.create Params.init_pointer in
          branch |> Codec.Branch.encode |> Params.branch_set p;
          Params.commit_set p (Some commit);
          let+ _ = Capability.call_for_value_exn t method_id req in
          ()
      end

      module Commit = struct
        let of_hash repo hash : commit Lwt.t =
          let open Raw.Client.Repo.CommitOfHash in
          Logs.info (fun l ->
              l "Commit.of_hash: %a" (Irmin.Type.pp Store.Hash.t) hash);
          let req, p = Capability.Request.create Params.init_pointer in
          Params.hash_set p (Codec.Hash.encode hash);
          Lwt.return
          @@ Capability.call_for_caps repo method_id req
               Results.commit_get_pipelined

        let hash commit =
          let open Raw.Client.Commit.Hash in
          Logs.info (fun l -> l "Commit.hash");
          let req = Capability.Request.create_no_args () in
          let+ res = Capability.call_for_value_exn commit method_id req in
          Results.hash_get res |> Codec.Hash.decode |> Result.get_ok

        let info commit =
          let open Raw.Client.Commit.Info in
          Logs.info (fun l -> l "Commit.info");
          let req = Capability.Request.create_no_args () in
          let+ res = Capability.call_for_value_exn commit method_id req in
          Results.info_get res |> Codec.Info.decode

        let tree commit =
          let open Raw.Client.Commit.Tree in
          Logs.info (fun l -> l "Commit.tree");
          let req = Capability.Request.create_no_args () in
          let+ res = Capability.call_for_value_exn commit method_id req in
          Results.tree_get res |> Codec.Tree.decode

        let parents commit =
          let open Raw.Client.Commit.Parents in
          Logs.info (fun l -> l "Commit.parents");
          let req = Capability.Request.create_no_args () in
          let+ res = Capability.call_for_value_exn commit method_id req in
          Results.hashes_get_list res
          |> List.map (fun x -> Codec.Hash.decode x |> Result.get_ok)
      end

      module Sync = struct
        type endpoint = Remote.t

        let clone t endpoint =
          let open Raw.Client.Sync.Clone in
          let (module Remote) = remote in
          Logs.info (fun l -> l "Sync.clone");
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Remote.encode endpoint);
          Capability.call_for_caps t method_id req Results.result_get_pipelined
          |> Lwt.return

        let pull t ~info endpoint =
          let open Raw.Client.Sync.Pull in
          let (module Remote) = remote in
          Logs.info (fun l -> l "Sync.pull");
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Remote.encode endpoint);
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
          let (module Remote) = remote in
          Logs.info (fun l -> l "Sync.push");
          let req, p = Capability.Request.create Params.init_pointer in
          Params.endpoint_set p (Remote.encode endpoint);
          let* (x : Results.t) =
            Capability.call_for_value_exn t method_id req
          in
          let x = Results.result_get x in
          decode_push_result x
      end

      module Pack = struct
        let integrity_check ?(auto_repair = false) t =
          let open Raw.Client.Pack.IntegrityCheck in
          Logs.info (fun l -> l "Pack.integrity_check");
          let req, p = Capability.Request.create Params.init_pointer in
          Params.auto_repair_set p auto_repair;
          Params.pack_set p (Some t);
          let* x = Capability.call_for_value_exn t method_id req in
          let results = Results.result_get x in
          Lwt.return
          @@
          let open Raw.Reader.Pack.IntegrityCheckResult in
          match Raw.Reader.Pack.IntegrityCheckResult.get results with
          | NoError -> Ok `No_error
          | Fixed n -> Ok (`Fixed (Int64.to_int n))
          | Corrupted n -> Error (`Corrupted (Int64.to_int n))
          | CannotFix m -> Error (`Cannot_fix m)
          | Undefined x -> failwith ("undefined: " ^ string_of_int x)
      end
    end

    let repo t =
      let open Raw.Client.Irmin.Repo in
      Logs.info (fun l -> l "Irmin.repo");
      let req = Capability.Request.create_no_args () in
      Capability.call_for_caps t method_id req Results.repo_get_pipelined
      |> Lwt.return

    let ping t =
      let open Raw.Client.Irmin.Ping in
      Logs.info (fun l -> l "Irmin.ping");
      let req = Capability.Request.create_no_args () in
      let+ res = Capability.call_for_value t method_id req in
      Result.map (fun _ -> ()) res
  end
