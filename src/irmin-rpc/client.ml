include Client_intf
open Capnp_rpc_lwt
open Utils
open Lwt.Syntax
open Lwt.Infix

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
    include Types
    module Key = Store.Key
    module Hash = Store.Hash

    type t = Raw.Client.Irmin.t Capability.t

    type branch = Store.branch

    type key = Store.key

    type contents = Store.contents

    type hash = Store.hash

    type step = Store.Key.step

    let remote =
      match Remote.v with
      | Some x -> x
      | None ->
          (module struct
            type t = Store.Private.Sync.endpoint

            let fail () = failwith "Sync API is not available"

            let decode _ = fail ()

            let encode _ = fail ()
          end)

    module Commit = struct
      type t = commit

      let check commit =
        let open Raw.Client.Commit.Check in
        Logs.info (fun l -> l "Commit.check");
        let req = Capability.Request.create_no_args () in
        Capability.call_for_value commit method_id req >|= function
        | Ok res -> Results.bool_get res
        | _ -> false

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
        Capability.call_for_caps commit method_id req Results.tree_get_pipelined
        |> Lwt.return

      let parents commit =
        let open Raw.Client.Commit.Parents in
        Logs.info (fun l -> l "Commit.parents");
        let req = Capability.Request.create_no_args () in
        let+ res = Capability.call_for_value_exn commit method_id req in
        Results.hashes_get_list res
        |> List.map (fun x -> Codec.Hash.decode x |> Result.get_ok)
    end

    module Sync = struct
      type t = sync

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
            let hash = Codec.Hash.decode head |> unwrap in
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
        let* (x : Results.t) = Capability.call_for_value_exn t method_id req in
        let x = Results.result_get x in
        decode_push_result x
    end

    module Tree = struct
      type t = tree

      type concrete = [ `Contents of hash | `Tree of (step * concrete) list ]

      let empty repo =
        let open Raw.Client.Repo.EmptyTree in
        Logs.info (fun l -> l "Tree.empty");
        let req = Capability.Request.create_no_args () in
        Capability.call_for_caps repo method_id req Results.tree_get_pipelined
        |> Lwt.return

      let check tree =
        let open Raw.Client.Tree.Check in
        Logs.info (fun l -> l "Tree.check");
        let req = Capability.Request.create_no_args () in
        Capability.call_for_value tree method_id req >|= function
        | Ok res -> Results.bool_get res
        | _ -> false

      let find tree key =
        let open Raw.Client.Tree.Find in
        Logs.info (fun l -> l "Tree.find");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let+ x = Capability.call_for_value_exn tree method_id req in
        if Results.has_contents x then
          Some (Results.contents_get x |> Codec.Contents.decode |> unwrap)
        else None

      let find_tree tree key =
        let open Raw.Client.Tree.FindTree in
        Logs.info (fun l -> l "Tree.find_tree");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let cap =
          Capability.call_for_caps tree method_id req Results.tree_get_pipelined
        in
        let+ ok = check cap in
        if ok then Some cap else None

      let get_tree t key =
        let open Raw.Client.Tree.FindTree in
        log_key (module Store) "Tree.find_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_caps t method_id req Results.tree_get_pipelined
        |> Lwt.return

      let add tree key value =
        let open Raw.Client.Tree.Add in
        Logs.info (fun l -> l "Tree.add");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        Params.contents_set p (Codec.Contents.encode value);
        Capability.call_for_caps tree method_id req Results.tree_get_pipelined
        |> Lwt.return

      let add_tree tree key value =
        let open Raw.Client.Tree.AddTree in
        Logs.info (fun l -> l "Tree.add_tree");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        Params.tree_set p (Some value);
        Capability.call_for_caps tree method_id req Results.tree_get_pipelined
        |> Lwt.return

      let mem tree key =
        let open Raw.Client.Tree.Mem in
        Logs.info (fun l -> l "Tree.mem");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let+ x = Capability.call_for_value_exn tree method_id req in
        Results.exists_get x

      let mem_tree tree key =
        let open Raw.Client.Tree.MemTree in
        Logs.info (fun l -> l "Tree.mem_tree");
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let+ x = Capability.call_for_value_exn tree method_id req in
        Results.exists_get x

      let concrete tree =
        let open Raw.Client.Tree.GetConcrete in
        Logs.info (fun l -> l "Tree.concrete");
        let req = Capability.Request.create_no_args () in
        let+ x = Capability.call_for_value_exn tree method_id req in
        let concrete = Results.concrete_get x in
        Codec.Tree.decode concrete

      let find_hash t key =
        let open Raw.Client.Tree.FindHash in
        log_key (module Store) "Tree.find_hash" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let+ res = Capability.call_for_value_exn t method_id req in
        match Results.has_hash res with
        | true -> Some (Results.hash_get res |> Codec.Hash.decode |> unwrap)
        | false -> None

      let remove t key =
        let open Raw.Client.Tree.Remove in
        log_key (module Store) "Tree.remove" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_caps t method_id req Results.tree_get_pipelined
        |> Lwt.return
    end

    module St = Store

    module Store = struct
      type t = store

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
        let cap =
          Capability.call_for_caps t method_id req Results.tree_get_pipelined
        in
        let+ ok = Tree.check cap in
        if ok then Some cap else None

      let get_tree t key =
        let open Raw.Client.Store.FindTree in
        log_key (module Store) "Store.find_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Capability.call_for_caps t method_id req Results.tree_get_pipelined
        |> Lwt.return

      let find_hash t key =
        let open Raw.Client.Store.FindHash in
        log_key (module Store) "Store.find_hash" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        let+ res = Capability.call_for_value_exn t method_id req in
        match Results.has_hash res with
        | true -> Some (Results.hash_get res |> Codec.Hash.decode |> unwrap)
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

      let test_and_set ~info t key ~test ~set =
        let open Raw.Client.Store.TestAndSet in
        log_key (module Store) "Store.test_and_set" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Option.iter
          (fun contents -> Codec.Contents.encode contents |> Params.test_set p)
          test;
        Option.iter
          (fun contents -> Codec.Contents.encode contents |> Params.set_set p)
          set;
        let+ x = Capability.call_for_value t method_id req in
        match x with Ok _ -> true | _ -> false

      let set_tree ~info t key tree =
        let open Raw.Client.Store.SetTree in
        log_key (module Store) "Store.set_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Codec.Key.encode key |> Params.key_set p;
        Params.tree_set p (Some tree);
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()

      let test_and_set_tree ~info t key ~test ~set =
        let open Raw.Client.Store.TestAndSetTree in
        log_key (module Store) "Store.test_and_set_tree" key;
        let req, p = Capability.Request.create Params.init_pointer in
        Params.key_set p (Codec.Key.encode key);
        let (_ : Raw.Builder.Info.t) =
          info () |> Codec.Info.encode |> Params.info_set_builder p
        in
        Params.test_set p test;
        Params.set_set p set;
        let+ x = Capability.call_for_value t method_id req in
        match x with Ok _ -> true | _ -> false

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
    end

    module Pack = struct
      type t = pack

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

    module Branch = struct
      include St.Branch

      let list t =
        let open Raw.Client.Repo.BranchList in
        Logs.info (fun l -> l "Branch.list");
        let req = Capability.Request.create_no_args () in
        let+ res = Capability.call_for_value_exn t method_id req in
        Results.branches_get_list res |> List.map (Codec.Branch.decode >> unwrap)

      let remove t branch =
        let open Raw.Client.Repo.BranchRemove in
        Logs.info (fun l ->
            l "Branch.remove: %a" (Irmin.Type.pp St.Branch.t) branch);
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()

      let set t branch commit =
        let open Raw.Client.Repo.BranchSet in
        Logs.info (fun l ->
            l "Branch.set: %a" (Irmin.Type.pp St.Branch.t) branch);
        let req, p = Capability.Request.create Params.init_pointer in
        branch |> Codec.Branch.encode |> Params.branch_set p;
        Params.commit_set p (Some commit);
        let+ _ = Capability.call_for_value_exn t method_id req in
        ()
    end

    module Contents = struct
      include St.Contents

      module Cache = Irmin.Private.Lru.Make (struct
        type t = hash

        let equal a b = Irmin.Type.unstage (Irmin.Type.equal St.Hash.t) a b

        let hash = St.Hash.short_hash
      end)

      let cache = Cache.create 16

      let of_hash repo hash =
        let open Raw.Client.Repo.ContentsOfHash in
        Logs.info (fun l -> l "Contents.of_hash");
        if Cache.mem cache hash then Lwt.return_some (Cache.find cache hash)
        else
          let req, p = Capability.Request.create Params.init_pointer in
          let () = Params.hash_set p (Codec.Hash.encode hash) in
          let+ x = Capability.call_for_value_exn repo method_id req in
          if Results.has_contents x then
            let c = Results.contents_get x in
            let c = Codec.Contents.decode c |> unwrap in
            let () = Cache.add cache hash c in
            Some c
          else None

      let find store key =
        let+ hash = Store.find_hash store key in
        match hash with
        | None -> None
        | Some x ->
            Some
              (fun repo ->
                let+ x = of_hash repo x in
                Option.get x)
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
