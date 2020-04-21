include Irmin_rpc_intf
open Lwt.Infix
open Capnp_rpc_lwt
module Client = Client

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

module Make (Store : Irmin.S) (Info : INFO) (Remote : REMOTE) = struct
  module Store = Store
  module Sync = Irmin.Sync (Store)
  module Codec = Codec.Make (Store)

  let local ctx =
    let module Ir = Raw.Service.Irmin in
    Ir.local
    @@ object
         inherit Ir.service

         method find_impl req release_params =
           let open Ir.Find in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let key =
             Params.key_get req |> Irmin.Type.of_string Store.key_t |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Store.find t key >>= function
               | Some value ->
                   Results.result_set results
                     (Irmin.Type.to_string Store.contents_t value);
                   Lwt.return_ok resp
               | None -> Lwt.return_ok resp)

         method set_impl req release_params =
           let open Ir.Set in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let key =
             Params.key_get req |> Irmin.Type.of_string Store.key_t |> unwrap
           in
           let value = Params.value_get req in
           let message =
             if Params.has_message req then Params.message_get req else "set"
           in
           let author =
             if Params.has_author req then Params.author_get req
             else "irmin-rpc"
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               match Irmin.Type.of_string Store.contents_t value with
               | Ok value -> (
                   Store.set_exn t key value
                     ~info:(Info.info ~author "%s" message)
                   >>= fun () ->
                   Store.Head.find t >>= function
                   | Some head ->
                       let commit = Results.result_init results in
                       Codec.encode_commit commit head >>= fun () ->
                       Lwt.return_ok resp
                   | None ->
                       let err =
                         Capnp_rpc.Exception.v ~ty:`Failed "Unable to set key"
                       in
                       Lwt.return_error (`Capnp (`Exception err)) )
               | Error (`Msg m) ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed m in
                   Lwt.return_error (`Capnp (`Exception err)))

         method remove_impl req release_params =
           let open Ir.Remove in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let key =
             Params.key_get req |> Irmin.Type.of_string Store.key_t |> unwrap
           in
           let message =
             if Params.has_message req then Params.message_get req else "remove"
           in
           let author =
             if Params.has_author req then Params.author_get req
             else "irmin-rpc"
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Store.remove_exn t key ~info:(Info.info ~author "%s" message)
               >>= fun () ->
               Store.Head.get t >>= fun head ->
               let commit = Results.result_init results in
               Codec.encode_commit commit head >>= fun () -> Lwt.return_ok resp)

         method find_tree_impl req release_params =
           let open Ir.FindTree in
           let module Tree = Raw.Builder.Irmin.Tree in
           let module Node = Raw.Builder.Irmin.Node in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let key =
             Params.key_get req |> Irmin.Type.of_string Store.key_t |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Store.find_tree t key >>= function
               | Some tree ->
                   let tr = Results.result_init results in
                   Codec.encode_tree tr key tree >>= fun () ->
                   Lwt.return_ok resp
               | None -> Lwt.return_ok resp)

         method set_tree_impl req release_params =
           let open Ir.SetTree in
           let module Tree = Raw.Builder.Irmin.Tree in
           let module Node = Raw.Builder.Irmin.Node in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let key =
             Params.key_get req |> Irmin.Type.of_string Store.key_t |> unwrap
           in
           let tree = Params.tree_get req in
           let message =
             if Params.has_message req then Params.message_get req
             else "set_tree"
           in
           let author =
             if Params.has_author req then Params.author_get req
             else "irmin-rpc"
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               let tree = Codec.decode_tree tree |> Store.Tree.of_concrete in
               Store.set_tree_exn t key tree
                 ~info:(Info.info ~author "%s" message)
               >>= fun () ->
               Store.Head.get t >>= fun head ->
               let commit = Results.result_init results in
               Codec.encode_commit commit head >>= fun () -> Lwt.return_ok resp)

         method clone_impl req release_params =
           let open Ir.Clone in
           let remote = Params.remote_get req in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Sync.fetch t (Remote.remote remote) >>= function
               | Ok (`Head head) ->
                   let commit = Results.result_init results in
                   Codec.encode_commit commit head >>= fun () ->
                   Lwt.return_ok resp
               | Ok `Empty -> Lwt.return_ok resp
               | Error (`Msg s) ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed s in
                   Lwt.return_error (`Capnp (`Exception err)))

         method push_impl req release_params =
           let open Ir.Push in
           let remote = Params.remote_get req |> Remote.remote in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, _result =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Sync.push t remote >>= function
               | Ok _ -> Lwt.return_ok resp
               | Error err ->
                   let err =
                     Fmt.to_to_string Sync.pp_push_error err
                     |> Capnp_rpc.Exception.v ~ty:`Failed
                   in
                   Lwt.return_error (`Capnp (`Exception err)))

         method pull_impl req release_params =
           let open Ir.Pull in
           let remote = Params.remote_get req |> Remote.remote in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let message =
             if Params.has_message req then Some (Params.message_get req)
             else None
           in
           let author =
             if Params.has_author req then Some (Params.author_get req)
             else None
           in
           release_params ();
           let info =
             match (message, author) with
             | None, None -> `Set
             | Some message, None -> `Merge (Info.info "%s" message)
             | None, Some author -> `Merge (Info.info ~author "merge")
             | Some message, Some author ->
                 `Merge (Info.info ~author "%s" message)
           in
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Sync.pull t remote info >>= function
               | Ok (`Head head) ->
                   let commit = Results.result_init results in
                   Codec.encode_commit commit head >>= fun () ->
                   Lwt.return_ok resp
               | Ok `Empty ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed "No head" in
                   Lwt.return_error (`Capnp (`Exception err))
               | Error (`Msg message) ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed message in
                   Lwt.return_error (`Capnp (`Exception err))
               | Error (`Conflict _) ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed "Conflict" in
                   Lwt.return_error (`Capnp (`Exception err)))

         method merge_impl req release_params =
           let open Ir.Merge in
           let from_ =
             Params.branch_from_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let into_ =
             Params.branch_into_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let message =
             if Params.has_message req then Params.message_get req else "merge"
           in
           let author =
             if Params.has_author req then Params.author_get req
             else "irmin-rpc"
           in
           release_params ();
           let info = Info.info ~author "%s" message in
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx into_ >>= fun t ->
               Store.merge_with_branch t from_ ~info >>= fun res ->
               match res with
               | Ok () ->
                   Store.Head.get t >>= fun head ->
                   let commit = Results.result_init results in
                   Codec.encode_commit commit head >>= fun () ->
                   Lwt.return_ok resp
               | Error e ->
                   let msg =
                     Fmt.to_to_string
                       (Irmin.Type.pp_json Irmin.Merge.conflict_t)
                       e
                   in
                   let err = Capnp_rpc.Exception.v ~ty:`Failed msg in
                   Lwt.return_error (`Capnp (`Exception err)))

         method commit_info_impl req release_params =
           let open Ir.CommitInfo in
           let hash = Params.hash_get req in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               let hash = Irmin.Type.of_string Store.Hash.t hash |> unwrap in
               Store.Commit.of_hash ctx hash >>= function
               | Some c ->
                   let info = Results.result_init results in
                   Codec.encode_commit_info c info;
                   Lwt.return_ok resp
               | None ->
                   let err =
                     Capnp_rpc.Exception.v ~ty:`Failed "Invalid commit"
                   in
                   Lwt.return_error (`Capnp (`Exception err)))

         method snapshot_impl req release_params =
           let open Ir.Snapshot in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               Store.of_branch ctx branch >>= fun t ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.Head.find t >>= function
               | Some commit ->
                   let s = Irmin.Type.to_string Store.Hash.t in
                   Results.result_set results (Store.Commit.hash commit |> s);
                   Lwt.return_ok resp
               | None ->
                   let err = Capnp_rpc.Exception.v ~ty:`Failed "No head" in
                   Lwt.return_error (`Capnp (`Exception err)))

         method revert_impl req release_params =
           let open Ir.Revert in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let commit =
             Params.hash_get req |> Irmin.Type.of_string Store.Hash.t |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               (Store.Commit.of_hash ctx commit >>= function
                | Some commit ->
                    Store.Head.set t commit >|= fun () ->
                    Results.result_set results true
                | None ->
                    Results.result_set results false;
                    Lwt.return_unit)
               >>= fun () -> Lwt.return_ok resp)

         method branches_impl _req release_params =
           let open Ir.Branches in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.Branch.list ctx >>= fun branches ->
               let l =
                 List.map
                   (fun x -> Irmin.Type.to_string Store.branch_t x)
                   branches
               in
               let (_
                     : ( Irmin_api.rw,
                         string,
                         Raw.Reader.builder_array_t )
                       Capnp.Array.t) =
                 Results.result_set_list results l
               in
               Lwt.return_ok resp)

         method commit_history_impl req release_params =
           let open Ir.CommitHistory in
           let commit =
             Params.hash_get req |> Irmin.Type.of_string Store.Hash.t |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               ( Store.Commit.of_hash ctx commit >>= fun commit ->
                 let l =
                   match commit with
                   | Some commit ->
                       Store.Commit.parents commit
                       |> List.map (Irmin.Type.to_string Store.Hash.t)
                   | None -> []
                 in
                 let (_
                       : ( Irmin_api.rw,
                           string,
                           Raw.Reader.builder_array_t )
                         Capnp.Array.t) =
                   Results.result_set_list results l
                 in
                 Lwt.return_unit )
               >>= fun () -> Lwt.return_ok resp)

         method remove_branch_impl req release_params =
           let open Ir.RemoveBranch in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               Store.Branch.remove ctx branch >>= fun () ->
               let resp, _results =
                 Service.Response.create Results.init_pointer
               in
               Lwt.return_ok resp)

         method create_branch_impl req release_params =
           let open Ir.CreateBranch in
           let branch =
             Params.branch_get req
             |> Irmin.Type.of_string Store.branch_t
             |> unwrap
           in
           let commit =
             Params.hash_get req |> Irmin.Type.of_string Store.Hash.t |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, _results =
                 Service.Response.create Results.init_pointer
               in
               Store.Commit.of_hash ctx commit >>= function
               | Some commit ->
                   Store.Branch.set ctx branch commit >>= fun () ->
                   Lwt.return_ok resp
               | None ->
                   let err =
                     Capnp_rpc.Exception.v ~ty:`Failed "Invalid commit"
                   in
                   Lwt.return_error (`Capnp (`Exception err)))
       end
end
