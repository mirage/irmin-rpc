include Irmin_rpc_intf
open Lwt.Infix
open Capnp_rpc_lwt
module Client = Client

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

let error m =
  let err = Capnp_rpc.Exception.v ~ty:`Failed m in
  Error (`Capnp (`Exception err))

let ignore_result_set r =
  ignore (r : (Irmin_api.rw, string, Raw.Reader.builder_array_t) Capnp.Array.t)

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
             Params.branch_get req |> Codec.Branch.decode |> unwrap
           in
           let key = Params.key_get req |> Codec.Key.decode |> unwrap in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Store.find t key >|= fun v ->
               Option.iter
                 (fun value ->
                   Codec.Contents.encode value |> Results.result_set results)
                 v;
               Ok resp)

         method set_impl req release_params =
           let open Ir.Set in
           let branch = Params.branch_get req |> Codec.Branch.decode |> unwrap
           and key = Params.key_get req |> Codec.Key.decode |> unwrap
           and value = Params.value_get req
           and message = Params.message_get req
           and author = Params.author_get req in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Codec.Contents.decode value
               |> Result.fold
                    ~error:(fun (`Msg m) -> error m |> Lwt.return)
                    ~ok:(fun value ->
                      Store.set_exn t key value
                        ~info:(Info.info ~author "%s" message)
                      >>= fun () ->
                      Store.Head.find t >>= function
                      | None -> error "Unable to set key" |> Lwt.return
                      | Some head ->
                          let commit = Results.result_init results in
                          Codec.encode_commit commit head >|= fun () -> Ok resp))

         method remove_impl req release_params =
           let open Ir.Remove in
           let branch = Params.branch_get req |> Codec.Branch.decode |> unwrap
           and key = Params.key_get req |> Codec.Key.decode |> unwrap
           and message = Params.message_get req
           and author = Params.author_get req in
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
             Params.branch_get req |> Codec.Branch.decode |> unwrap
           in
           let key = Params.key_get req |> Codec.Key.decode |> unwrap in
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
           let branch = Params.branch_get req |> Codec.Branch.decode |> unwrap
           and key = Params.key_get req |> Codec.Key.decode |> unwrap
           and tree = Params.tree_get req
           and message = Params.message_get req
           and author = Params.author_get req in
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
             Params.branch_get req |> Codec.Branch.decode |> unwrap
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
               | Error (`Msg s) -> Lwt.return (error s))

         method push_impl req release_params =
           let open Ir.Push in
           let remote = Params.remote_get req |> Remote.remote
           and branch =
             Params.branch_get req |> Codec.Branch.decode |> unwrap
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
                   Fmt.to_to_string Sync.pp_push_error err
                   |> error
                   |> Lwt.return)

         method pull_impl req release_params =
           let open Ir.Pull in
           let remote = Params.remote_get req |> Remote.remote
           and branch = Params.branch_get req |> Codec.Branch.decode |> unwrap
           and message = Params.message_get req
           and author = Params.author_get req in
           release_params ();
           let info = `Merge (Info.info ~author "%s" message) in
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Sync.pull t remote info >>= function
               | Ok (`Head head) ->
                   let commit = Results.result_init results in
                   Codec.encode_commit commit head >|= fun () -> Ok resp
               | Ok `Empty -> error "No head" |> Lwt.return
               | Error (`Msg message) -> error message |> Lwt.return
               | Error (`Conflict _) -> error "Conflict" |> Lwt.return)

         method merge_impl req release_params =
           let open Ir.Merge in
           let from =
             Params.branch_from_get req |> Codec.Branch.decode |> unwrap
           and into =
             Params.branch_into_get req |> Codec.Branch.decode |> unwrap
           and message = Params.message_get req
           and author = Params.author_get req in
           release_params ();
           let info = Info.info ~author "%s" message in
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx into >>= fun t ->
               Store.merge_with_branch t from ~info
               >>= Result.fold
                     ~error:(fun e ->
                       Fmt.to_to_string
                         (Irmin.Type.pp_json Irmin.Merge.conflict_t)
                         e
                       |> error
                       |> Lwt.return)
                     ~ok:(fun () ->
                       Store.Head.get t >>= fun head ->
                       let commit = Results.result_init results in
                       Codec.encode_commit commit head >|= fun () -> Ok resp))

         method commit_info_impl req release_params =
           let open Ir.CommitInfo in
           let hash = Params.hash_get req in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               let hash = Codec.Hash.decode hash |> unwrap in
               Store.Commit.of_hash ctx hash
               >|= Option.fold ~none:(error "Invalid commit")
                     ~some:(fun commit ->
                       Results.result_init results
                       |> Codec.encode_commit_info commit;
                       Ok resp))

         method snapshot_impl req release_params =
           let open Ir.Snapshot in
           let branch =
             Params.branch_get req |> Codec.Branch.decode |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               Store.of_branch ctx branch >>= fun t ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.Head.find t
               >|= Option.fold ~none:(error "No head") ~some:(fun commit ->
                       Store.Commit.hash commit
                       |> Codec.Hash.encode
                       |> Results.result_set results;
                       Ok resp))

         method revert_impl req release_params =
           let open Ir.Revert in
           let branch =
             Params.branch_get req |> Codec.Branch.decode |> unwrap
           in
           let commit = Params.hash_get req |> Codec.Hash.decode |> unwrap in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.of_branch ctx branch >>= fun t ->
               Store.Commit.of_hash ctx commit
               >>= Option.fold ~none:Lwt.return_false ~some:(fun c ->
                       Store.Head.set t c >|= fun () -> true)
               >|= fun b ->
               Results.result_set results b;
               Ok resp)

         method branches_impl _req release_params =
           let open Ir.Branches in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.Branch.list ctx >|= fun branches ->
               List.map Codec.Branch.encode branches
               |> Results.result_set_list results
               |> ignore_result_set;
               Ok resp)

         method commit_history_impl req release_params =
           let open Ir.CommitHistory in
           let commit = Params.hash_get req |> Codec.Hash.decode |> unwrap in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, results =
                 Service.Response.create Results.init_pointer
               in
               Store.Commit.of_hash ctx commit >|= fun commit ->
               Option.fold ~none:[] ~some:Store.Commit.parents commit
               |> List.map Codec.Hash.encode
               |> Results.result_set_list results
               |> ignore_result_set;
               Ok resp)

         method remove_branch_impl req release_params =
           let open Ir.RemoveBranch in
           let branch =
             Params.branch_get req |> Codec.Branch.decode |> unwrap
           in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, _results =
                 Service.Response.create Results.init_pointer
               in
               Store.Branch.remove ctx branch >|= fun () -> Ok resp)

         method create_branch_impl req release_params =
           let open Ir.CreateBranch in
           let branch = Params.branch_get req |> Codec.Branch.decode |> unwrap
           and commit = Params.hash_get req |> Codec.Hash.decode |> unwrap in
           release_params ();
           Service.return_lwt (fun () ->
               let resp, _results =
                 Service.Response.create Results.init_pointer
               in
               Store.Commit.of_hash ctx commit
               >>= Option.fold
                     ~none:(error "Invalid commit" |> Lwt.return)
                     ~some:(fun c ->
                       Store.Branch.set ctx branch c >|= fun () -> Ok resp))
       end
end
