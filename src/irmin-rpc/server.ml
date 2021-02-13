open Server_intf
open Capnp_rpc_lwt
open Lwt.Syntax
open Lwt.Infix
open Utils

let ( let+! ) x f = Lwt.map (Result.map f) x

module type S = S

module type MAKER = MAKER

module Log = (val Logs.src_log (Logs.Src.create "irmin.rpc" ~doc:"Irmin RPC")
                : Logs.LOG)

let ignore_result_set r =
  ignore (r : (Irmin_api.rw, string, Raw.Reader.builder_array_t) Capnp.Array.t)

module type RESULTS = sig
  type t

  val init_pointer : Raw.Builder.pointer_t -> t
end

let with_initialised_results (type t) (module Results : RESULTS with type t = t)
    f =
  let response, results = Service.Response.create Results.init_pointer in
  Service.return_lwt (fun () ->
      let+ x = f results in
      Result.map (fun () -> response) x)

module Make : MAKER =
functor
  (St : Irmin.S)
  (Remote : Config_intf.REMOTE with type t = St.Private.Sync.endpoint)
  (Pack : Config_intf.PACK with type repo = St.repo)
  ->
  struct
    module P = Pack

    module Context = struct
      type t = { tx : (Raw.Client.Tx.t cap, St.tree ref) Hashtbl.t }

      let empty () = { tx = Hashtbl.create 8 }
    end

    let remote =
      match Remote.v with
      | Some x -> x
      | None ->
          (module struct
            type t = St.Private.Sync.endpoint

            let decode _ = assert false

            let encode _ = assert false
          end)

    module Sy = Irmin.Sync (St)
    module Codec = Codec.Make (St)

    type repo = St.repo

    type store = St.t

    type commit = St.commit

    type hash = St.hash

    let unwrap = function
      | Ok x -> x
      | Error (`Msg m) -> failwith m
      | Error (`Commit_not_found h) ->
          Fmt.failwith "Commit not found: %a" (Irmin.Type.pp St.Hash.t) h

    let convert_error pp =
      Fmt.strf "%a" pp >> Capnp_rpc.Exception.v ~ty:`Failed >> fun e ->
      `Capnp (`Exception e)

    let process_write_error =
      Lwt.map
        (Result.map_error (convert_error (Irmin.Type.pp St.write_error_t)))

    module Tree = struct
      type t = Raw.Client.Tree.t cap

      let trees = Hashtbl.create 8

      let rec local' tree =
        let module Tree = Raw.Service.Tree in
        object
          inherit Tree.service

          method find_impl params release_param_caps =
            let open Tree.Find in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.find");
            with_initialised_results
              (module Results)
              (fun results ->
                let* c = St.Tree.find tree (unwrap key) in
                let () =
                  Option.iter
                    (fun x ->
                      Results.contents_set results (Codec.Contents.encode x))
                    c
                in
                Lwt.return (Ok ()))

          method find_hash_impl params release_param_caps =
            let open Tree.FindHash in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Tree.find_hash" key;
            with_initialised_results
              (module Results)
              (fun results ->
                let+ x = St.Tree.find tree (unwrap key) in
                Option.iter
                  (fun x ->
                    let hash = St.Contents.hash x in
                    Codec.Hash.encode hash |> Results.hash_set results)
                  x;
                Ok ())

          method find_tree_impl params release_param_caps =
            let open Tree.FindTree in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.find_tree");
            with_initialised_results
              (module Results)
              (fun results ->
                let* (c : St.tree option) =
                  St.Tree.find_tree tree (unwrap key)
                in
                Option.iter
                  (fun c ->
                    let x = local c in
                    Results.tree_set results (Some x);
                    Capability.dec_ref x)
                  c;
                Lwt.return (Ok ()))

          method mem_impl params release_param_caps =
            let open Tree.Mem in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.mem");
            with_initialised_results
              (module Results)
              (fun results ->
                let* e = St.Tree.mem tree (unwrap key) in
                Results.exists_set results e;
                Lwt.return_ok ())

          method mem_tree_impl params release_param_caps =
            let open Tree.MemTree in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.mem_tree");
            with_initialised_results
              (module Results)
              (fun results ->
                let* e = St.Tree.mem_tree tree (unwrap key) in
                Results.exists_set results e;
                Lwt.return (Ok ()))

          method get_concrete_impl _params release_param_caps =
            let open Tree.GetConcrete in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.get_concrete");
            with_initialised_results
              (module Results)
              (fun results ->
                let* c = Codec.Tree.of_irmin_tree tree in
                let* c = Codec.Tree.encode c in
                ignore (Results.concrete_set_builder results c);
                Lwt.return (Ok ()))

          method hash_impl _params release_param_caps =
            let open Tree.Hash in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.hash");
            with_initialised_results
              (module Results)
              (fun results ->
                let hash = St.Tree.hash tree in
                Results.hash_set results (Codec.Hash.encode hash);
                Lwt.return @@ Ok ())

          method list_impl params release_param_caps =
            let open Tree.List in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Tree.list" key;
            with_initialised_results
              (module Results)
              (fun results ->
                let key = unwrap key in
                let* tree = St.Tree.list tree key in
                let l =
                  List.map (fun (step, _) -> Codec.Key.Step.encode step) tree
                in
                Results.items_set_list results l |> ignore;
                Lwt.return @@ Ok ())

          method check_impl _params release_param_caps =
            let open Tree.Check in
            release_param_caps ();
            Logs.info (fun f -> f "Tree.check");
            with_initialised_results
              (module Results)
              (fun results ->
                Results.bool_set results true;
                Lwt.return_ok ())
        end
        |> Tree.local

      and local (tree : St.tree) : t =
        let x = local' tree in
        Hashtbl.replace trees x tree;
        Capability.when_released x (fun () -> Hashtbl.remove trees x);
        x

      let empty = local St.Tree.empty

      let empty () =
        Capability.inc_ref empty;
        empty
    end

    module Tx = struct
      type t = Raw.Client.Tx.t cap

      let rec local' _client repo (tree : St.tree ref) =
        let module Tx = Raw.Service.Tx in
        object
          inherit Tx.service

          method add_impl params release_param_caps =
            let open Tx.Add in
            let key = Params.key_get params |> Codec.Key.decode in
            let contents = Params.contents_get params in
            release_param_caps ();
            log_key_result (module St) "Tx.add" key;
            with_initialised_results
              (module Results)
              (fun _results ->
                let contents = contents |> Codec.Contents.decode |> unwrap in
                let+ tree' = St.Tree.add !tree (unwrap key) contents in
                tree := tree';
                Ok ())

          method add_contents_impl params release_param_caps =
            let open Tx.AddContents in
            let key = Params.key_get params |> Codec.Key.decode in
            let hash = Params.hash_get params in
            release_param_caps ();
            log_key_result (module St) "Tx.add_contents" key;
            with_initialised_results
              (module Results)
              (fun _results ->
                let* contents =
                  hash
                  |> Codec.Hash.decode
                  |> unwrap
                  |> St.Contents.of_hash repo
                in
                let+ tree' =
                  St.Tree.add !tree (unwrap key) (Option.get contents)
                in
                tree := tree';
                Ok ())

          method add_tree_impl params release_param_caps =
            let open Tx.AddTree in
            let key = Params.key_get params |> Codec.Key.decode in
            let tr = Params.tree_get params in
            release_param_caps ();
            log_key_result (module St) "Tx.add_tree" key;
            with_initialised_results
              (module Results)
              (fun _results ->
                let tr = Hashtbl.find Tree.trees (Option.get tr) in
                let* tree' = St.Tree.add_tree !tree (unwrap key) tr in
                tree := tree';
                Lwt.return_ok ())

          method remove_impl params release_param_caps =
            let open Tx.Remove in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Tx.remove" key;
            with_initialised_results
              (module Results)
              (fun _results ->
                let* tree' = St.Tree.remove !tree (unwrap key) in
                tree := tree';
                Lwt.return_ok ())

          method tree_impl _params release_param_caps =
            let open Tx.Tree in
            release_param_caps ();
            Logs.info (fun f -> f "Tx.tree");
            with_initialised_results
              (module Results)
              (fun results ->
                let tree = Tree.local !tree in
                Results.tree_set results (Some tree);
                Capability.dec_ref tree;
                Lwt.return_ok ())
        end
        |> Tx.local

      and tx_limit = 32

      and local client repo (tree : St.tree) : t =
        (* TODO: make this limit configurable *)
        if Hashtbl.length client.Context.tx > tx_limit then
          failwith "Too many transactions"
        else
          let r = ref tree in
          let x = local' client repo r in
          Hashtbl.replace client.Context.tx x r;
          Capability.when_released x (fun () ->
              Hashtbl.remove client.Context.tx x);
          x
    end

    module Commit = struct
      type t = Raw.Client.Commit.t cap

      let read repo t =
        let open Raw.Client.Commit.Read in
        let req = Capability.Request.create_no_args () in
        let* str = Capability.call_for_value_exn t method_id req in
        Codec.Commit.decode repo (Results.value_get str)

      let local _client commit =
        let module Commit = Raw.Service.Commit in
        object
          inherit Commit.service

          method tree_impl _params release_param_caps =
            let open Commit.Tree in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.tree");
            with_initialised_results
              (module Results)
              (fun results ->
                let tree = St.Commit.tree commit in
                let tree = Tree.local tree in
                let _ = Results.tree_set results (Some tree) in
                Capability.dec_ref tree;
                Lwt.return_ok ())

          method parents_impl _params release_param_caps =
            let open Commit.Parents in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.parents");
            with_initialised_results
              (module Results)
              (fun results ->
                let parents =
                  St.Commit.parents commit |> List.map Codec.Hash.encode
                in
                ignore_result_set (Results.hashes_set_list results parents);
                Lwt.return_ok ())

          method info_impl _params release_param_caps =
            let open Commit.Info in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.info");
            with_initialised_results
              (module Results)
              (fun results ->
                let (_ : Raw.Builder.Info.t) =
                  St.Commit.info commit
                  |> Codec.Info.encode
                  |> Results.info_set_builder results
                in
                Lwt.return_ok ())

          method hash_impl _params release_param_caps =
            let open Commit.Hash in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.hash");
            with_initialised_results
              (module Results)
              (fun results ->
                let hash = St.Commit.hash commit |> Codec.Hash.encode in
                Results.hash_set results hash;
                Lwt.return_ok ())

          method read_impl _params release_param_caps =
            let open Commit.Read in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.read");
            with_initialised_results
              (module Results)
              (fun results ->
                let+ (_ : Raw.Builder.Commit.Value.t) =
                  commit
                  |> Codec.Commit.encode
                  >|= Results.value_set_builder results
                in
                Ok ())

          method check_impl _params release_param_caps =
            let open Commit.Check in
            release_param_caps ();
            Logs.info (fun f -> f "Commit.check");
            with_initialised_results
              (module Results)
              (fun results ->
                Results.bool_set results true;
                Lwt.return_ok ())
        end
        |> Commit.local
    end

    module Pack = struct
      let local _client repo =
        let (module P) = Option.get Pack.v in
        let repo : P.repo = Obj.magic repo in
        let module Pack = Raw.Service.Pack in
        object
          inherit Pack.service

          method integrity_check_impl params release_param_caps =
            let open Pack.IntegrityCheck in
            let auto_repair = Params.auto_repair_get params in
            release_param_caps ();
            with_initialised_results
              (module Results)
              (fun results ->
                let chk = P.integrity_check ~auto_repair repo in
                let inner =
                  Raw.Builder.Pack.IntegrityCheckResult.init_root ()
                in
                let () =
                  match chk with
                  | Ok `No_error ->
                      Raw.Builder.Pack.IntegrityCheckResult.no_error_set inner
                  | Ok (`Fixed n) ->
                      Raw.Builder.Pack.IntegrityCheckResult.fixed_set_int inner
                        n
                  | Error (`Corrupted n) ->
                      Raw.Builder.Pack.IntegrityCheckResult.corrupted_set_int
                        inner n
                  | Error (`Cannot_fix m) ->
                      Raw.Builder.Pack.IntegrityCheckResult.cannot_fix_set inner
                        m
                in
                ignore (Results.result_set_builder results inner);
                Lwt.return @@ Ok ())
        end
        |> Pack.local
    end

    module Sync = struct
      let remote_of_endpoint e = St.E e

      module type RESULT_SET = sig
        type t

        val result_set : t -> Raw.Client.Commit.t cap option -> unit
      end

      let handle_pull client (type t)
          (module Results : RESULT_SET with type t = t) store results = function
        | Ok _ ->
            St.Head.find store >|= fun head ->
            let () =
              match head with
              | Some head ->
                  let commit = Commit.local client head in
                  Results.result_set results (Some commit);
                  Capability.dec_ref commit
              | _ -> ()
            in
            Ok ()
        | Error e -> Lwt.return @@ Error (convert_error Sy.pp_pull_error e)

      let local client store =
        let module Sync = Raw.Service.Sync in
        object
          inherit Sync.service

          method push_impl params release_param_caps =
            let open Sync.Push in
            let endpoint = Params.endpoint_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Sync.push");
            let (module Remote) = remote in
            with_initialised_results
              (module Results)
              (fun results ->
                let remote =
                  endpoint |> Remote.decode |> unwrap |> remote_of_endpoint
                in
                let+ (_ : Raw.Builder.Sync.PushResult.t) =
                  Sy.push store remote
                  >>= Codec.Push_result.encode
                  >|= Results.result_set_builder results
                in
                Ok ())

          method pull_impl params release_param_caps =
            let open Sync.Pull in
            let endpoint = Params.endpoint_get params in
            let info = Codec.Info.decode (Params.info_get params) in
            release_param_caps ();
            Logs.info (fun f -> f "Sync.pull");
            let (module Remote) = remote in
            with_initialised_results
              (module Results)
              (fun results ->
                let remote =
                  endpoint |> Remote.decode |> unwrap |> remote_of_endpoint
                in
                Sy.pull store remote (`Merge (fun () -> info))
                >>= handle_pull client
                      (module Results : RESULT_SET with type t = Results.t)
                      store results)

          method clone_impl params release_param_caps =
            let open Sync.Clone in
            let endpoint = Params.endpoint_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Sync.clone");
            let (module Remote) = remote in
            with_initialised_results
              (module Results)
              (fun results ->
                let remote =
                  endpoint |> Remote.decode |> unwrap |> remote_of_endpoint
                in
                Sy.pull store remote `Set
                >>= handle_pull client
                      (module Results : RESULT_SET with type t = Results.t)
                      store results)
        end
        |> Sync.local
    end

    module Store = struct
      type t = Raw.Client.Store.t cap

      let local client store =
        let module Store = Raw.Service.Store in
        object
          inherit Store.service

          method find_impl params release_param_caps =
            let open Store.Find in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.find" key;
            with_initialised_results
              (module Results)
              (fun results ->
                let+ x = St.find store (unwrap key) in
                Option.iter
                  (fun x ->
                    Codec.Contents.encode x |> Results.contents_set results)
                  x;
                Ok ())

          method find_hash_impl params release_param_caps =
            let open Store.FindHash in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.find_hash" key;
            with_initialised_results
              (module Results)
              (fun results ->
                let+ x = St.find store (unwrap key) in
                Option.iter
                  (fun x ->
                    let hash = St.Contents.hash x in
                    Codec.Hash.encode hash |> Results.hash_set results)
                  x;
                Ok ())

          method find_tree_impl params release_param_caps =
            let open Store.FindTree in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.find_tree" key;
            with_initialised_results
              (module Results)
              (fun results ->
                let+ () =
                  St.find_tree store (unwrap key) >|= fun tree ->
                  Option.iter
                    (fun tree ->
                      let x = Tree.local tree in
                      let () = Results.tree_set results (Some x) in
                      Capability.dec_ref x)
                    tree
                in
                Ok ())

          method set_impl params release_param_caps =
            let open Store.Set in
            let key = Params.key_get params |> Codec.Key.decode in
            let info = Params.info_get params
            and contents = Params.contents_get params in
            release_param_caps ();
            log_key_result (module St) "Store.set" key;
            Service.return_lwt (fun () ->
                let info = info |> Codec.Info.decode
                and contents = contents |> Codec.Contents.decode |> unwrap in
                let+! () =
                  St.set ~info:(fun () -> info) store (unwrap key) contents
                  |> process_write_error
                in
                Service.Response.create_empty ())

          method set_tree_impl params release_param_caps =
            let open Store.SetTree in
            let key = Params.key_get params |> Codec.Key.decode in
            let info = Params.info_get params
            and tree = Params.tree_get params in
            release_param_caps ();
            log_key_result (module St) "Store.set_tree" key;
            Service.return_lwt (fun () ->
                let info = info |> Codec.Info.decode in
                let tree = Hashtbl.find Tree.trees (Option.get tree) in
                let+! () =
                  St.set_tree ~info:(fun () -> info) store (unwrap key) tree
                  |> process_write_error
                in
                Service.Response.create_empty ())

          method remove_impl params release_param_caps =
            let open Store.Remove in
            let key = Params.key_get params |> Codec.Key.decode
            and info = Params.info_get params in
            release_param_caps ();
            log_key_result (module St) "Store.remove" key;
            Service.return_lwt (fun () ->
                let info = info |> Codec.Info.decode in
                let+! () =
                  St.remove ~info:(fun () -> info) store (unwrap key)
                  |> process_write_error
                in
                Service.Response.create_empty ())

          method mem_impl params release_param_caps =
            let open Store.Mem in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.mem" key;
            with_initialised_results
              (module Results)
              (fun results ->
                St.mem store (unwrap key) >|= fun exists ->
                Results.exists_set results exists;
                Ok ())

          method mem_tree_impl params release_param_caps =
            let open Store.MemTree in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.mem" key;
            with_initialised_results
              (module Results)
              (fun results ->
                St.mem_tree store (unwrap key) >|= fun exists ->
                Results.exists_set results exists;
                Ok ())

          method merge_with_branch_impl params release_param_caps =
            let open Store.MergeWithBranch in
            let branch = Params.branch_get params
            and info = Params.info_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Store.merge_into: :%s" branch);
            with_initialised_results
              (module Results)
              (fun results ->
                let branch = branch |> Codec.Branch.decode |> unwrap
                and info = info |> Codec.Info.decode in
                let b_merge_result =
                  Raw.Builder.Store.MergeResult.init_root ()
                in
                let+ () =
                  St.merge_with_branch store ~info:(fun () -> info) branch
                  >|= Codec.Merge_result.encode b_merge_result
                in
                let (_ : Raw.Builder.Store.MergeResult.t) =
                  Results.result_set_builder results b_merge_result
                in
                Ok ())

          method sync_impl _params release_param_caps =
            let open Store.Sync in
            release_param_caps ();
            Logs.info (fun f -> f "Store.sync");
            with_initialised_results
              (module Results)
              (fun results ->
                if Option.is_some Remote.v then (
                  let cap = Sync.local client store in
                  Results.sync_set results (Some cap);
                  Capability.dec_ref cap);
                Lwt.return @@ Ok ())

          method pack_impl _params release_param_caps =
            let open Store.Pack in
            release_param_caps ();
            Logs.info (fun f -> f "Store.pack");
            with_initialised_results
              (module Results)
              (fun results ->
                if Option.is_some P.v then (
                  let cap = Pack.local client (St.repo store) in
                  Results.pack_set results (Some cap);
                  Capability.dec_ref cap);
                Lwt.return @@ Ok ())

          method last_modified_impl params release_param_caps =
            let open Store.LastModified in
            let key = Params.key_get params |> Codec.Key.decode in
            release_param_caps ();
            log_key_result (module St) "Store.last_modified" key;
            with_initialised_results
              (module Results)
              (fun results ->
                St.last_modified ~n:1 store (unwrap key) >|= function
                | [] -> Ok ()
                | x :: _ ->
                    let commit = Commit.local client x in
                    Results.commit_set results (Some commit);
                    Capability.dec_ref commit;
                    Ok ())

          method test_and_set_impl params release_param_caps =
            let open Store.TestAndSet in
            let key = Params.key_get params |> Codec.Key.decode in
            let info = Params.info_get params
            and test =
              if Params.has_test params then Some (Params.test_get params)
              else None
            and set =
              if Params.has_set params then Some (Params.set_get params)
              else None
            in
            release_param_caps ();
            log_key_result (module St) "Store.test_and_set" key;
            Service.return_lwt (fun () ->
                let info = info |> Codec.Info.decode
                and test =
                  Option.map (fun x -> Codec.Contents.decode x |> unwrap) test
                and set =
                  Option.map (fun x -> Codec.Contents.decode x |> unwrap) set
                in
                let+! () =
                  St.test_and_set
                    ~info:(fun () -> info)
                    store (unwrap key) ~test ~set
                  |> process_write_error
                in
                Service.Response.create_empty ())

          method test_and_set_tree_impl params release_param_caps =
            let open Store.TestAndSetTree in
            let key = Params.key_get params |> Codec.Key.decode in
            let info = Params.info_get params
            and test = Params.test_get params
            and set = Params.set_get params in
            release_param_caps ();
            log_key_result (module St) "Store.test_and_set_tree" key;
            Service.return_lwt (fun () ->
                let get x = Hashtbl.find Tree.trees x in
                let info = info |> Codec.Info.decode
                and test = Option.map get test
                and set = Option.map get set in
                let+! () =
                  St.test_and_set_tree
                    ~info:(fun () -> info)
                    store (unwrap key) ~test ~set
                  |> process_write_error
                in
                Service.Response.create_empty ())
        end
        |> Store.local
    end

    module Repo = struct
      type t = Raw.Client.Repo.t cap

      module BranchMap = Map.Make (struct
        type t = St.Branch.t

        let compare a b =
          (Irmin.Type.unstage @@ Irmin.Type.compare St.Branch.t) a b
      end)

      let local client repo =
        let module Repo = Raw.Service.Repo in
        object
          inherit Repo.service

          method master_impl _params release_param_caps =
            let open Repo.Master in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.master");
            with_initialised_results
              (module Results)
              (fun results ->
                let+ store = St.master repo in
                let store_service = Store.local client store in
                Results.store_set results (Some store_service);
                Capability.dec_ref store_service;
                Ok ())

          method of_branch_impl params release_param_caps =
            let open Repo.OfBranch in
            let branch = Params.branch_get params in
            release_param_caps ();
            Log.info (fun f -> f "Repo.of_branch: %s" branch);
            with_initialised_results
              (module Results)
              (fun results ->
                let branch = branch |> Codec.Branch.decode |> unwrap in
                let+ store = St.of_branch repo branch in
                let store_service = Store.local client store in
                Results.store_set results (Some store_service);
                Capability.dec_ref store_service;
                Ok ())

          method branch_list_impl _params release_param_caps =
            let open Repo.BranchList in
            release_param_caps ();
            Log.info (fun f -> f "Repo.branch_list");
            with_initialised_results
              (module Results)
              (fun results ->
                let+ branches = St.Repo.branches repo in
                let branches = List.map Codec.Branch.encode branches in
                Results.branches_set_list results branches |> ignore_result_set;
                Ok ())

          method branch_remove_impl params release_param_caps =
            let open Repo.BranchRemove in
            let branch = Params.branch_get params in
            release_param_caps ();
            Log.info (fun f -> f "Repo.branch_remove: %s" branch);
            Service.return_lwt (fun () ->
                let branch = branch |> Codec.Branch.decode |> unwrap in
                let+ () = St.Branch.remove repo branch in
                Ok (Service.Response.create_empty ()))

          method branch_set_impl params release_param_caps =
            let open Repo.BranchSet in
            let branch = Params.branch_get params in
            let commit = Params.commit_get params |> Option.get in
            release_param_caps ();
            Service.return_lwt (fun () ->
                let branch = branch |> Codec.Branch.decode |> unwrap in
                let* commit = Commit.read repo commit >|= unwrap in
                let+ () = St.Branch.set repo branch commit in
                Ok (Service.Response.create_empty ()))

          method commit_of_hash_impl params release_param_caps =
            let open Repo.CommitOfHash in
            let hash = Params.hash_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.commit_of_hash: %s" hash);
            with_initialised_results
              (module Results)
              (fun results ->
                let hash = hash |> Codec.Hash.decode |> unwrap in
                let+ commit = St.Commit.of_hash repo hash in
                let commit =
                  Option.map (fun c -> Commit.local client c) commit
                in
                Results.commit_set results commit;
                Option.iter Capability.dec_ref commit;
                Ok ())

          method contents_of_hash_impl params release_param_caps =
            let open Repo.ContentsOfHash in
            let hash = Params.hash_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.contents_of_hash: %s" hash);
            with_initialised_results
              (module Results)
              (fun results ->
                let hash = hash |> Codec.Hash.decode |> unwrap in
                let+ contents = St.Contents.of_hash repo hash in
                Option.iter
                  (fun c ->
                    let s = Codec.Contents.encode c in
                    Results.contents_set results s)
                  contents;
                Ok ())

          method empty_tree_impl _params release_param_caps =
            let open Repo.EmptyTree in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.empty_tree");
            with_initialised_results
              (module Results)
              (fun results ->
                let t = Tree.empty () in
                Results.tree_set results (Some t);
                Capability.dec_ref t;
                Lwt.return_ok ())

          method create_commit_impl params release_param_caps =
            let open Repo.CreateCommit in
            let info = Params.info_get params in
            let parents = Params.parents_get_list params in
            let tree = Params.tree_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.create_commit");
            with_initialised_results
              (module Results)
              (fun results ->
                Logs.info (fun l -> l "XXX");
                let parents =
                  List.map (fun x -> Codec.Hash.decode x |> unwrap) parents
                in
                let info = Codec.Info.decode info in
                let tree = Hashtbl.find Tree.trees (Option.get tree) in
                let* commit = St.Commit.v repo ~info ~parents tree in
                let commit = Commit.local client commit in
                Results.commit_set results (Some commit);
                Capability.dec_ref commit;
                Lwt.return_ok ())

          method import_contents_impl params release_param_caps =
            let open Repo.ImportContents in
            let contents = Params.values_get_list params in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.import_contents");
            with_initialised_results
              (module Results)
              (fun results ->
                let* slice = St.Private.Slice.empty () in
                let* hashes =
                  Lwt_list.map_s
                    (fun contents ->
                      let contents = Codec.Contents.decode contents |> unwrap in
                      let hash = St.Contents.hash contents in
                      let+ () =
                        St.Private.Slice.add slice (`Contents (hash, contents))
                      in
                      Codec.Hash.encode hash)
                    contents
                in
                let* () = St.Repo.import repo slice >|= unwrap in
                Results.hash_set_list results hashes |> ignore;
                Lwt.return_ok ())

          method tree_of_concrete_impl params release_param_caps =
            let open Repo.TreeOfConcrete in
            let concrete = Params.concrete_get params in
            release_param_caps ();
            Logs.info (fun f -> f "Repo.tree_of_concrete");
            with_initialised_results
              (module Results)
              (fun results ->
                let concrete = Codec.Tree.decode concrete in
                let+ tree = Codec.Tree.to_irmin_tree repo concrete in
                let tree = Tree.local tree in
                Results.tree_set results (Some tree);
                Capability.dec_ref tree;
                Ok ())

          method tx_impl params release_param_caps =
            let open Repo.Tx in
            let tree = Params.tree_get params in
            release_param_caps ();
            Logs.info (fun l -> l "Repo.tx");
            with_initialised_results
              (module Results)
              (fun results ->
                let tree =
                  try Hashtbl.find Tree.trees (Option.get tree)
                  with Not_found -> St.Tree.empty
                in
                let tx = Tx.local client repo tree in
                Results.tx_set results (Some tx);
                Lwt.return_ok ())
        end
        |> Repo.local
    end

    let local ctx =
      let module I = Raw.Service.Irmin in
      object
        inherit I.service

        val repo_service =
          let client = Context.empty () in
          Repo.local client ctx

        method repo_impl _params release_param_caps =
          let open I.Repo in
          release_param_caps ();
          Logs.info (fun f -> f "Irmin.repo");
          let response, results =
            Service.Response.create Results.init_pointer
          in
          Capability.inc_ref repo_service;
          Results.repo_set results (Some repo_service);
          Service.return response

        method ping_impl _params release_param_caps =
          release_param_caps ();
          Logs.info (fun f -> f "Irmin.ping");
          let response = Service.Response.create_empty () in
          Service.return response
      end
      |> I.local
  end
