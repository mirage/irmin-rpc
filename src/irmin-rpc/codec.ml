include Codec_intf
open Lwt.Infix

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

module Make (Store : Irmin.S) = struct
  let rec encode_tree tr key (tree : Store.tree) : unit Lwt.t =
    let module Tree = Raw.Builder.Irmin.Tree in
    let module Node = Raw.Builder.Irmin.Node in
    let ks = Irmin.Type.to_string Store.key_t key in
    ignore @@ Tree.key_set tr ks;
    Store.Tree.to_concrete tree >>= function
    | `Contents (contents, _) ->
        let _ =
          Tree.contents_set tr (Irmin.Type.to_string Store.contents_t contents)
        in
        Lwt.return_unit
    | `Tree l ->
        Lwt_list.map_p
          (fun (step, tree) ->
            let node = Node.init_root () in
            let step_s = Irmin.Type.to_string Store.step_t step in
            Node.step_set node step_s;
            let tt = Node.tree_init node in
            let tree = Store.Tree.of_concrete tree in
            encode_tree tt (Store.Key.rcons key step) tree >|= fun () -> node)
          l
        >>= fun l ->
        let _ = Tree.node_set_list tr l in
        Lwt.return_unit

  let rec decode_tree tree : Store.Tree.concrete =
    let module Tree = Raw.Reader.Irmin.Tree in
    let module Node = Raw.Reader.Irmin.Node in
    match Tree.get tree with
    | Node l ->
        let l = Capnp.Array.to_list l in
        `Tree
          (List.map
             (fun node ->
               let step =
                 Node.step_get node
                 |> Irmin.Type.of_string Store.step_t
                 |> unwrap
               in
               let tree = Node.tree_get node |> decode_tree in
               (step, tree))
             l)
    | Contents c ->
        let c = Irmin.Type.of_string Store.contents_t c |> unwrap in
        `Contents (c, Store.Metadata.default)
    | Undefined _ -> `Tree []

  let encode_commit_info cm info =
    let module Info = Raw.Builder.Irmin.Info in
    let i = Store.Commit.info cm in
    Info.author_set info (Irmin.Info.author i);
    Info.message_set info (Irmin.Info.message i);
    Info.date_set info (Irmin.Info.date i)

  let encode_commit commit cm =
    let module Commit = Raw.Builder.Irmin.Commit in
    let module Info = Raw.Builder.Irmin.Info in
    let info = Commit.info_init commit in
    let hash = Irmin.Type.to_string Store.Hash.t (Store.Commit.hash cm) in
    Commit.hash_set commit hash;
    let tr = Commit.tree_init commit in
    let tree = Store.Commit.tree cm in
    encode_tree tr Store.Key.empty tree >|= fun () -> encode_commit_info cm info
end
