open Lwt.Infix
open Capnp_rpc_lwt

module Api = Irmin_api.MakeRPC(Capnp_rpc_lwt)
type t = [ `Irmin_b2b5cb4fd15c7d5a ] Capability.t

module type CLIENT = sig
    module Store: Irmin.S
    val get: t -> ?branch:Store.branch -> Store.key -> (Store.contents, [`Msg of string]) result Lwt.t
    val set: t -> ?branch:Store.branch -> ?author:string -> ?message:string -> Store.key -> Store.contents -> Store.Commit.hash Lwt.t
    val remove: t -> ?branch:Store.branch -> ?author:string -> ?message:string -> Store.key -> Store.Commit.hash Lwt.t
    val clone: t -> ?branch:Store.branch -> string -> Store.Commit.hash Lwt.t
    val pull: t -> ?branch:Store.branch -> ?author:string -> ?message:string -> string -> Store.Commit.hash Lwt.t
    val push: t -> ?branch:Store.branch -> string -> unit Lwt.t
    val merge: t -> ?branch:Store.branch -> ?author:string -> ?message:string -> Store.branch -> (Store.Commit.hash, Irmin.Merge.conflict) result Lwt.t
end

module type S = sig
  module Store: Irmin.S

  val local: Store.repo -> t

  module Client: CLIENT with module Store = Store
end

exception Error_message of string
let unwrap = function
  | Ok x -> x
  | Error (`Msg m) -> raise (Error_message m)

module Make(Store: Irmin.S)(Info: sig
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end) = struct
  module Store = Store
  module Sync = Irmin.Sync(Store)

  let local ctx =
    let module Ir = Api.Service.Irmin in

    let to_commit commit cm =
        let module Commit = Api.Builder.Irmin.Commit in
        let module Info = Api.Builder.Irmin.Info in
        let info = Commit.info_init commit in
        Commit.hash_set commit (Fmt.to_to_string Store.Commit.Hash.pp (Store.Commit.hash cm));
        let i = Store.Commit.info cm in
        Info.author_set info (Irmin.Info.author i);
        Info.message_set info (Irmin.Info.message i);
        Info.date_set info (Irmin.Info.date i)
    in

    (* Get branch by name and convert it to a capnproto Branch object *)
    let to_branch ?branch br =
        let module Branch = Api.Builder.Irmin.Branch in
        let commit = Branch.head_init br in
        let get_branch () = match branch with None -> Store.master ctx | Some name -> Store.of_branch ctx name in
        get_branch ()>>= fun t ->
        Store.Head.find t >>= function
        | Some head ->
          Branch.name_set br "master" |> ignore;
          to_commit commit head;
          Lwt.return_some br
        | None -> Lwt.return_none
    in

    (* Convert a Store.tree to capnproto Tree object *)
    let rec encode_tree tr key (tree: Store.tree): unit Lwt.t =
      let module Tree = Api.Builder.Irmin.Tree in
      let module Node = Api.Builder.Irmin.Node in
      let ks = Fmt.to_to_string Store.Key.pp key in
      ignore @@ Tree.key_set tr ks;
      Store.Tree.to_concrete tree >>= function
      | `Contents (contents, _) ->
          let _ = Tree.contents_set tr (Fmt.to_to_string Store.Contents.pp contents) in
          Lwt.return_unit
      | `Tree l ->
          Lwt_list.map_p (fun (step, tree) ->
            let node = Node.init_root () in
            let step_s = Fmt.to_to_string Store.Key.pp_step step in
            Node.step_set node step_s;
            let tt = Node.tree_init node in
            let tree = Store.Tree.of_concrete tree in
            encode_tree tt (Store.Key.rcons key step) tree >|= fun () -> node
          ) l
          >>= fun l ->
            let _ = Tree.node_set_list tr l in Lwt.return_unit
    in

    let rec decode_tree tree: Store.Tree.concrete =
      let module Tree = Api.Reader.Irmin.Tree in
      let module Node = Api.Reader.Irmin.Node in
      match Tree.get tree with
      | Node l ->
          let l = Capnp.Array.to_list l in
          `Tree (List.map (fun node ->
            let step = Node.step_get node |> Store.Key.step_of_string |> unwrap in
            let tree = Node.tree_get node |> decode_tree in
            step, tree) l)
      | Contents c ->
          let c = Store.Contents.of_string c |> unwrap in
          `Contents (c, Store.Metadata.default)
      | Undefined _ -> `Tree []
    in

    Ir.local @@ object
      inherit Ir.service

      method get_impl req release_params =
        let open Ir.Get in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let key = Params.key_get req
          |> Store.Key.of_string
          |> unwrap
        in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Store.find t key >>= fun value ->
            begin
              match value with
              | Some value ->
                Results.result_set results (Fmt.to_to_string Store.Contents.pp value)
              | None -> ()
            end;
          Lwt.return_ok resp)

      method set_impl req release_params =
        let open Ir.Set in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let key = Params.key_get req |> Store.Key.of_string |> unwrap in
        let value = Params.value_get req in
        let message = if Params.has_message req then Params.message_get req else "set" in
        let author = if Params.has_author req then Params.author_get req else "irmin-rpc" in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          (match Store.Contents.of_string value with
          | Ok value ->
            Store.set t key value ~info:(Info.info ~author "%s" message) >>= fun () ->
            Store.Head.get t >>= fun head ->
            let commit = Results.result_init results in
            to_commit commit head;
            Lwt.return_unit
          | Error _ -> Lwt.return_unit) >>= fun _ ->
          Lwt.return_ok resp)

      method remove_impl req release_params =
        let open Ir.Remove in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let key = Params.key_get req |> Store.Key.of_string |> unwrap in
        let message = if Params.has_message req then Params.message_get req else "remove" in
        let author = if Params.has_author req then Params.author_get req else "irmin-rpc" in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Store.remove t key ~info:(Info.info ~author "%s" message) >>= fun () ->
          Store.Head.get t >>= fun head ->
          let commit = Results.result_init results in
          to_commit commit head;
          Lwt.return_ok resp)

      method master_impl _req release_params =
        let open Ir.Master in
        let module Branch = Api.Builder.Irmin.Branch in
        let module Commit = Api.Builder.Irmin.Commit in
        let module Info = Api.Builder.Irmin.Info in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          let br = Results.result_init results in
          to_branch br >>= fun _ -> Lwt.return_ok resp)

      method get_branch_impl req release_params =
        let open Ir.GetBranch in
        let module Branch = Api.Builder.Irmin.Branch in
        let module Commit = Api.Builder.Irmin.Commit in
        let module Info = Api.Builder.Irmin.Info in
        let name = Params.name_get req |> Store.Branch.of_string |> unwrap in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          let br = Results.result_init results in
          to_branch ~branch:name br >>= fun _ -> Lwt.return_ok resp)

      method get_tree_impl req release_params =
        let open Ir.GetTree in
        let module Tree = Api.Builder.Irmin.Tree in
        let module Node = Api.Builder.Irmin.Node in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let key = Params.key_get req |> Store.Key.of_string |> unwrap in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Store.get_tree t key >>= fun tree ->
          let tr = Results.result_init results in
          encode_tree tr key tree >>= fun () ->
          Lwt.return_ok resp)

      method set_tree_impl req release_params =
        let open Ir.SetTree in
        let module Tree = Api.Builder.Irmin.Tree in
        let module Node = Api.Builder.Irmin.Node in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let key = Params.key_get req |> Store.Key.of_string |> unwrap in
        let tree = Params.tree_get req in
        let message = if Params.has_message req then Params.message_get req else "remove" in
        let author = if Params.has_author req then Params.author_get req else "irmin-rpc" in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          let tree = decode_tree tree |> Store.Tree.of_concrete in
          Store.set_tree t key tree ~info:(Info.info ~author "%s" message) >>= fun () ->
          Store.Head.get t >>= fun head ->
          let commit = Results.result_init results in
          to_commit commit head;
          Lwt.return_ok resp)

      method clone_impl req release_params =
        let open Ir.Clone in
        let remote = Params.remote_get req |> Irmin.remote_uri in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Sync.fetch_exn t remote >>= fun head ->
          let commit = Results.result_init results in
          to_commit commit head;
          Lwt.return_ok resp
        )

      method push_impl req release_params =
        let open Ir.Push in
        let remote = Params.remote_get req |> Irmin.remote_uri in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        release_params ();
        Service.return_lwt (fun () ->
          let resp, _result = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Sync.push_exn t remote >>= fun () ->
          Lwt.return_ok resp)

      method pull_impl req release_params =
        let open Ir.Pull in
        let remote = Params.remote_get req |> Irmin.remote_uri in
        let branch = Params.branch_get req |> Store.Branch.of_string |> unwrap in
        let message = if Params.has_message req then Params.message_get req else "remove" in
        let author = if Params.has_author req then Params.author_get req else "irmin-rpc" in
        release_params ();
        let info = Info.info ~author "%s" message in
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx branch >>= fun t ->
          Sync.pull_exn t remote (`Merge info) >>= fun () ->
          Store.Head.get t >>= fun head ->
          let commit = Results.result_init results in
          to_commit commit head;
          Lwt.return_ok resp
        )

      method merge_impl req release_params =
        let open Ir.Merge in
        let from_ = Params.branch_from_get req |> Store.Branch.of_string |> unwrap in
        let into_ = Params.branch_into_get req |> Store.Branch.of_string |> unwrap in
        let message = if Params.has_message req then Params.message_get req else "remove" in
        let author = if Params.has_author req then Params.author_get req else "irmin-rpc" in
        release_params ();
        let info = Info.info ~author "%s" message in
        Service.return_lwt (fun () ->
          let resp, results = Service.Response.create Results.init_pointer in
          Store.of_branch ctx into_ >>= fun t ->
          Store.merge_with_branch t from_  ~info >>= fun res ->
          match res with
          | Ok () ->
            Store.Head.get t >>= fun head ->
            let commit = Results.result_init results in
            to_commit commit head;
            Lwt.return_ok resp
          | Error e ->
              let msg = (Fmt.to_to_string (Irmin.Type.pp_json Irmin.Merge.conflict_t) e) in
              let err = Capnp_rpc.Error.exn ~ty:`Failed "%s" msg in
              Lwt.return_error err
        )
    end

    module Client = struct
      module Store = Store
      module Ir = Api.Client.Irmin

      let get t ?branch key =
        let open Ir.Get in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        let key_s = Fmt.to_to_string Store.Key.pp key in
        Params.key_set p key_s |> ignore;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        Store.Contents.of_string (Results.result_get res)

      let set t ?branch ?author ?message key value =
        let open Ir.Set in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        (match author with
        | Some author ->
            Params.author_set p author
        | _ -> ());
        (match message with
        | Some message ->
            Params.message_set p message
        | _ -> ());
        let key_s = Fmt.to_to_string Store.Key.pp key in
        Params.key_set p key_s |> ignore;
        Params.value_set p (Fmt.to_to_string Store.Contents.pp value);
        Capability.call_for_value_exn t method_id req >|= fun res ->
        let commit = Results.result_get res in
        Api.Reader.Irmin.Commit.hash_get commit |> Store.Commit.Hash.of_string |> unwrap

      let remove t ?branch ?author ?message key =
        let open Ir.Remove in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        (match author with
        | Some author ->
            Params.author_set p author
        | _ -> ());
        (match message with
        | Some message ->
            Params.message_set p message
        | _ -> ());
        let key_s = Fmt.to_to_string Store.Key.pp key in
        Params.key_set p key_s |> ignore;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        let commit = Results.result_get res in
        (Api.Reader.Irmin.Commit.hash_get commit |> Store.Commit.Hash.of_string |> unwrap)

      let clone t ?branch remote =
        let open Ir.Clone in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        Params.remote_set p remote;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        let commit = Results.result_get res in
        (Api.Reader.Irmin.Commit.hash_get commit |> Store.Commit.Hash.of_string |> unwrap)

      let pull t ?branch ?author ?message remote =
        let open Ir.Pull in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        (match author with
        | Some author ->
            Params.author_set p author
        | _ -> ());
        (match message with
        | Some message ->
            Params.message_set p message
        | _ -> ());
        Params.remote_set p remote;
        Capability.call_for_value_exn t method_id req >|= fun res ->
        let commit = Results.result_get res in
        (Api.Reader.Irmin.Commit.hash_get commit |> Store.Commit.Hash.of_string |> unwrap)

      let push t ?branch  remote =
        let open Ir.Push in
         let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_set p br
        | None -> ());
        Params.remote_set p remote;
        Capability.call_for_unit_exn t method_id req

      let merge t ?branch ?author ?message from_ =
        let open Ir.Merge in
        let req, p = Capability.Request.create Params.init_pointer in
        (match branch with
        | Some br ->
            let br = Fmt.to_to_string Store.Branch.pp br in
            Params.branch_into_set p br
        | None -> ());
        let from_ = Fmt.to_to_string Store.Branch.pp from_ in
        Params.branch_from_set p from_;
        (match author with
        | Some author ->
            Params.author_set p author
        | _ -> ());
        (match message with
        | Some message ->
            Params.message_set p message
        | _ -> ());
        Capability.call_for_value t method_id req >|= fun res ->
        match res with
        | Ok res ->
            let commit = Results.result_get res in
            Ok (Api.Reader.Irmin.Commit.hash_get commit |> Store.Commit.Hash.of_string |> unwrap)
        | Error err ->
            let err = Fmt.to_to_string Capnp_rpc.Error.pp err in
            let decoder = Jsonm.decoder (`String err) in
            Error (Irmin.Type.decode_json Irmin.Merge.conflict_t decoder |> unwrap)
    end
end

