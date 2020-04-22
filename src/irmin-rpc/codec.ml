include Codec_intf
open Lwt.Infix

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

let codec_of_type (type a) (t : a Irmin.Type.t) =
  let encode = Irmin.Type.to_string t
  and decode s =
    ( Irmin.Type.of_string t s
      : (a, [ `Msg of _ ]) result
      :> (a, [> `Msg of _ ]) result )
  in
  (encode, decode)

module Make (Store : Irmin.S) = struct
  module Branch = struct
    type t = Store.branch

    let encode, decode = codec_of_type Store.Branch.t
  end

  module Key = struct
    type t = Store.key

    let encode, decode = codec_of_type Store.Key.t
  end

  module Hash = struct
    type t = Store.hash

    let encode, decode = codec_of_type Store.Hash.t
  end

  module Contents = struct
    type t = Store.contents

    let encode, decode = codec_of_type Store.Contents.t
  end

  let rec encode_tree tr key (tree : Store.tree) : unit Lwt.t =
    let module Tree = Raw.Builder.Irmin.Tree in
    let module Node = Raw.Builder.Irmin.Node in
    Irmin.Type.to_string Store.key_t key |> Tree.key_set tr;
    Store.Tree.to_concrete tree >>= function
    | `Contents (contents, _) ->
        Tree.contents_set tr (Irmin.Type.to_string Store.contents_t contents);
        Lwt.return_unit
    | `Tree l ->
        Lwt_list.map_p
          (fun (step, tree) ->
            let node = Node.init_root () in
            Irmin.Type.to_string Store.step_t step |> Node.step_set node;
            let tt = Node.tree_init node in
            let tree = Store.Tree.of_concrete tree in
            encode_tree tt (Store.Key.rcons key step) tree >|= fun () -> node)
          l
        >>= fun l ->
        let (_
              : ( Irmin_rpc__Irmin_api.rw,
                  Irmin_rpc__Raw.Builder.Irmin.Node.t,
                  Raw.Reader.builder_array_t )
                Capnp.Array.t) =
          Tree.node_set_list tr l
        in
        Lwt.return_unit

  let rec decode_tree tree =
    let module Tree = Raw.Reader.Irmin.Tree in
    let module Node = Raw.Reader.Irmin.Node in
    match Tree.get tree with
    | Node l ->
        Capnp.Array.to_list l
        |> List.map (fun node ->
               let step =
                 Node.step_get node
                 |> Irmin.Type.of_string Store.step_t
                 |> unwrap
               in
               let tree = Node.tree_get node |> decode_tree in
               (step, tree))
        |> fun t -> `Tree t
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
    Store.Commit.hash cm
    |> Irmin.Type.to_string Store.Hash.t
    |> Commit.hash_set commit;
    let tr = Commit.tree_init commit in
    let tree = Store.Commit.tree cm in
    encode_tree tr Store.Key.empty tree >|= fun () -> encode_commit_info cm info
end
