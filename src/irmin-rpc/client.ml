include Client_intf
open Capnp_rpc_lwt
open Lwt.Infix

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

module Make (Store : Irmin.S) = struct
  module Store = Store
  module Ir = Raw.Client.Irmin
  module Codec = Codec.Make (Store)

  type t = capability

  let branch_param branch_set p branch =
    match branch with
    | Some br ->
        let br = Irmin.Type.to_string Store.branch_t br in
        branch_set p br
    | None -> branch_set p "master"

  let author_param author_set p author =
    match author with Some author -> author_set p author | _ -> ()

  let message_param message_set p message =
    match message with Some message -> message_set p message | _ -> ()

  let find t ?branch key =
    let open Ir.Find in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_set p branch;
    let key_s = Irmin.Type.to_string Store.key_t key in
    Params.key_set p key_s |> ignore;
    Capability.call_for_value_exn t method_id req >|= fun res ->
    if Results.has_result res then
      Some
        ( Irmin.Type.of_string Store.contents_t (Results.result_get res)
        |> unwrap )
    else None

  let get t ?branch key =
    find t ?branch key >|= function
    | Some x -> x
    | None -> raise (Error_message "Not found")

  let set t ?branch ?author ?message key value =
    let open Ir.Set in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_set p branch;
    author_param Params.author_set p author;
    message_param Params.message_set p message;
    let key_s = Irmin.Type.to_string Store.key_t key in
    Params.key_set p key_s |> ignore;
    Params.value_set p (Irmin.Type.to_string Store.contents_t value);
    Capability.call_for_value_exn t method_id req >|= fun res ->
    if Results.has_result res then
      let commit = Results.result_get res in
      Raw.Reader.Irmin.Commit.hash_get commit
      |> Irmin.Type.of_string Store.Hash.t
      |> unwrap
    else raise (Error_message "Unable to set key")

  let remove t ?branch ?author ?message key =
    let open Ir.Remove in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_set p branch;
    author_param Params.author_set p author;
    message_param Params.message_set p message;
    let key_s = Irmin.Type.to_string Store.key_t key in
    Params.key_set p key_s |> ignore;
    Capability.call_for_value_exn t method_id req >|= fun res ->
    let commit = Results.result_get res in
    Raw.Reader.Irmin.Commit.hash_get commit
    |> Irmin.Type.of_string Store.Hash.t
    |> unwrap

  let merge t ?branch ?author ?message from_ =
    let open Ir.Merge in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_into_set p branch;
    let from_ = Irmin.Type.to_string Store.branch_t from_ in
    Params.branch_from_set p from_;
    author_param Params.author_set p author;
    message_param Params.message_set p message;
    Capability.call_for_value t method_id req >|= fun res ->
    match res with
    | Ok res ->
        let commit = Results.result_get res in
        Ok
          ( Raw.Reader.Irmin.Commit.hash_get commit
          |> Irmin.Type.of_string Store.Hash.t
          |> unwrap )
    | Error (`Capnp err) ->
        let err = Fmt.to_to_string Capnp_rpc.Error.pp err in
        Error (`Msg err)

  let snapshot ?branch t =
    let open Ir.Snapshot in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_set p branch;
    Capability.call_for_value_exn t method_id req >|= fun res ->
    if Results.has_result res then
      let commit = Results.result_get res in
      Some (Irmin.Type.of_string Store.Hash.t commit |> unwrap)
    else None

  let revert t ?branch hash =
    let open Ir.Revert in
    let req, p = Capability.Request.create Params.init_pointer in
    branch_param Params.branch_set p branch;
    Params.hash_set p (Irmin.Type.to_string Store.Hash.t hash);
    Capability.call_for_value_exn t method_id req >|= fun res ->
    Results.result_get res

  module Tree = struct
    let set t ?branch ?author ?message key tree =
      let open Ir.SetTree in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p branch;
      author_param Params.author_set p author;
      message_param Params.message_set p message;
      let key_s = Irmin.Type.to_string Store.key_t key in
      Params.key_set p key_s |> ignore;
      let tr = Params.tree_init p in
      Codec.encode_tree tr key tree >>= fun () ->
      Capability.call_for_value_exn t method_id req >|= fun res ->
      let commit = Results.result_get res in
      Raw.Reader.Irmin.Commit.hash_get commit
      |> Irmin.Type.of_string Store.Hash.t
      |> unwrap

    let find t ?branch key =
      let open Ir.FindTree in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p branch;
      let key_s = Irmin.Type.to_string Store.key_t key in
      Params.key_set p key_s |> ignore;
      Capability.call_for_value_exn t method_id req >|= fun res ->
      if Results.has_result res then
        Some
          (Results.result_get res |> Codec.decode_tree |> Store.Tree.of_concrete)
      else None

    let get t ?branch key =
      find t ?branch key >|= function
      | Some x -> x
      | None -> raise (Error_message "Not found")
  end

  module Sync = struct
    let clone t ?branch remote =
      let open Ir.Clone in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p branch;
      Params.remote_set p remote;
      Capability.call_for_value t method_id req >|= function
      | Ok res ->
          let commit = Results.result_get res in
          Ok
            ( Raw.Reader.Irmin.Commit.hash_get commit
            |> Irmin.Type.of_string Store.Hash.t
            |> unwrap )
      | Error (`Capnp err) ->
          let s = Fmt.to_to_string Capnp_rpc.Error.pp err in
          Error (`Msg s)

    let pull t ?branch ?author ?message remote =
      let open Ir.Pull in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p branch;
      author_param Params.author_set p author;
      message_param Params.message_set p message;
      Params.remote_set p remote;
      Capability.call_for_value t method_id req >|= function
      | Ok res ->
          let commit = Results.result_get res in
          Ok
            ( Raw.Reader.Irmin.Commit.hash_get commit
            |> Irmin.Type.of_string Store.Hash.t
            |> unwrap )
      | Error (`Capnp err) ->
          let s = Fmt.to_to_string Capnp_rpc.Error.pp err in
          Error (`Msg s)

    let push t ?branch remote =
      let open Ir.Push in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p branch;
      Params.remote_set p remote;
      Capability.call_for_unit t method_id req >|= function
      | Ok () -> Ok ()
      | Error (`Capnp err) ->
          let s = Fmt.to_to_string Capnp_rpc.Error.pp err in
          Error (`Msg s)
  end

  module Commit = struct
    let info t hash =
      let open Ir.CommitInfo in
      let req, p = Capability.Request.create Params.init_pointer in
      Params.hash_set p (Irmin.Type.to_string Store.Hash.t hash);
      Capability.call_for_value_exn t method_id req >|= fun res ->
      if Results.has_result res then
        let info = Results.result_get res in
        let module Info = Raw.Reader.Irmin.Info in
        let author = Info.author_get info in
        let date = Info.date_get info in
        let message = Info.message_get info in
        Some (Irmin.Info.v ~date ~author message)
      else None

    let history t hash =
      let open Ir.CommitHistory in
      let req, p = Capability.Request.create Params.init_pointer in
      Params.hash_set p (Irmin.Type.to_string Store.Hash.t hash);
      Capability.call_for_value_exn t method_id req >>= fun res ->
      let l = Results.result_get_list res in
      Lwt_list.filter_map_s
        (fun x ->
          match Irmin.Type.of_string Store.Hash.t x with
          | Ok b -> Lwt.return_some b
          | Error _ -> Lwt.return_none)
        l
  end

  module Branch = struct
    let remove t branch =
      let open Ir.RemoveBranch in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p (Some branch);
      Capability.call_for_unit_exn t method_id req

    let create t name hash =
      let open Ir.CreateBranch in
      let req, p = Capability.Request.create Params.init_pointer in
      branch_param Params.branch_set p (Some name);
      Params.hash_set p (Irmin.Type.to_string Store.Hash.t hash);
      Capability.call_for_unit_exn t method_id req

    let list t =
      let open Ir.Branches in
      let req, _ = Capability.Request.create Params.init_pointer in
      Capability.call_for_value_exn t method_id req >>= fun res ->
      let l = Results.result_get_list res in
      Lwt_list.filter_map_s
        (fun x ->
          match Irmin.Type.of_string Store.branch_t x with
          | Ok b -> Lwt.return_some b
          | Error _ -> Lwt.return_none)
        l
  end
end
