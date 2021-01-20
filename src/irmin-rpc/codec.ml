include Codec_intf
open Lwt.Syntax

exception Error_message of string

let unwrap = function Ok x -> x | Error (`Msg m) -> raise (Error_message m)

let errorf fmt = Format.kasprintf (fun m -> Error (`Msg m)) fmt

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

  module Info = struct
    type t = Irmin.Info.t

    let encode : t -> Raw.Builder.Info.t =
     fun t ->
      let open Raw.Builder.Info in
      let b = init_root () in
      author_set b (Irmin.Info.author t);
      message_set b (Irmin.Info.message t);
      date_set b (Irmin.Info.date t);
      b

    let decode : Raw.Reader.Info.t -> t =
     fun str ->
      let open Raw.Reader.Info in
      let author = author_get str
      and message = message_get str
      and date = date_get str in
      Irmin.Info.v ~date ~author message
  end

  module Tree = struct
    type t = [ `Contents of Store.hash | `Tree of (Store.step * t) list ]

    (*let rec encode (tree : Store.tree) : Raw.Builder.Tree.t Lwt.t =
      let module B = Raw.Builder in
      let module R = Raw.Reader in
      let contents key value tr =
        Irmin.Type.to_string Store.key_t key |> B.Tree.key_set tr;
        Irmin.Type.to_string Store.Hash.t (Store.Contents.hash value)
        |> B.Tree.contents_set tr;
        Lwt.return tr
      in
      let node key node tr =
        Irmin.Type.to_string Store.key_t key |> B.Tree.key_set tr;
        let* items =
          Store.Tree.list (Store.Tree.of_node node) Store.Key.empty
        in
        let* l =
          Lwt_list.map_p
            (fun (step, tree) ->
              let node = B.Node.init_root () in
              Irmin.Type.to_string Store.step_t step |> B.Node.step_set node;
              let+ x = encode tree in
              ignore (B.Node.tree_set_builder node x);
              node)
            items
        in
        let (_ : (Irmin_api.rw, B.Node.t, R.builder_array_t) Capnp.Array.t) =
          B.Tree.node_set_list tr l
        in
        Lwt.return tr
      in
      let tr = Raw.Builder.Tree.init_root () in
      Store.Tree.fold tree ~contents ~node tr*)
    let rec of_irmin_tree (x : Store.tree) : t Lwt.t =
      let* t = Store.Tree.to_concrete x in
      match t with
      | `Contents (c, _) -> Lwt.return @@ `Contents (Store.Contents.hash c)
      | `Tree l ->
          let+ x =
            Lwt_list.map_s
              (fun (step, tree) ->
                let tree = Store.Tree.of_concrete tree in
                let+ x = of_irmin_tree tree in
                (step, x))
              l
          in
          `Tree x

    let to_irmin_tree repo (t : t) : Store.tree Lwt.t =
      let rec inner repo t =
        match t with
        | `Contents hash ->
            let+ contents = Store.Contents.of_hash repo hash in
            `Contents (Option.get contents, Store.Metadata.default)
        | `Tree l ->
            let+ l =
              Lwt_list.map_s
                (fun (step, t) ->
                  let+ t = inner repo t in
                  (step, t))
                l
            in
            `Tree l
      in
      let+ x = inner repo t in
      Store.Tree.of_concrete x

    let encode (tree : t) : Raw.Builder.Tree.Concrete.t Lwt.t =
      let rec inner tr key (tree : t) =
        let module B = Raw.Builder in
        let module R = Raw.Reader in
        Irmin.Type.to_string Store.key_t key |> B.Tree.Concrete.key_set tr;
        match tree with
        | `Contents hash ->
            let s = Irmin.Type.to_string Store.Hash.t hash in
            ignore (B.Tree.Concrete.contents_set tr s);
            Lwt.return_unit
        | `Tree l ->
            let* l =
              Lwt_list.map_p
                (fun (step, tree) ->
                  let node = B.Tree.Node.init_root () in
                  Irmin.Type.to_string Store.step_t step
                  |> B.Tree.Node.step_set node;
                  let tt = B.Tree.Concrete.init_root () in
                  let+ () = inner tt (Store.Key.rcons key step) tree in
                  node)
                l
            in
            let (_
                  : ( Irmin_api.rw,
                      B.Tree.Node.t,
                      R.builder_array_t )
                    Capnp.Array.t) =
              B.Tree.Concrete.node_set_list tr l
            in
            Lwt.return_unit
      in
      let tr = Raw.Builder.Tree.Concrete.init_root () in
      let+ () = inner tr Store.Key.empty tree in
      tr

    let decode (tree : Raw.Reader.Tree.Concrete.t) : t =
      let rec inner tree =
        let module Tree = Raw.Reader.Tree in
        let module Node = Raw.Reader.Tree.Node in
        match Tree.Concrete.get tree with
        | Node l ->
            Capnp.Array.to_list l
            |> List.map (fun node ->
                   let step =
                     Node.step_get node
                     |> Irmin.Type.of_string Store.step_t
                     |> unwrap
                   in
                   let tree = Node.tree_get node |> inner in
                   (step, tree))
            |> fun t -> `Tree t
        | Contents c ->
            let hash = Irmin.Type.of_string Store.Hash.t c |> unwrap in
            `Contents hash
        | Undefined _ -> `Tree []
      in
      inner tree
  end

  module Commit = struct
    type t = Store.commit

    let encode : t -> Raw.Builder.Commit.Value.t Lwt.t =
     fun t ->
      let open Raw.Builder.Commit.Value in
      let b = Raw.Builder.Commit.Value.init_root () in
      hash_set b (Store.Commit.hash t |> Hash.encode);
      let (_ : Raw.Builder.Info.t) =
        info_set_builder b (Store.Commit.info t |> Info.encode)
      in
      ignore
        (parents_set_list b (Store.Commit.parents t |> List.map Hash.encode));
      let* x = Store.Commit.tree t |> Tree.of_irmin_tree in
      let* t = Tree.encode x in
      ignore (tree_set_builder b t);
      Lwt.return b

    let decode :
        Store.repo ->
        Raw.Reader.Commit.Value.t ->
        (t, [> `Msg of string | `Commit_not_found of Store.hash ]) result Lwt.t
        =
     fun repo str ->
      let open Raw.Reader.Commit.Value in
      match hash_get str |> Hash.decode with
      | Ok hash -> (
          let+ commit = Store.Commit.of_hash repo hash in
          match commit with
          | Some c -> Ok c
          | None -> Error (`Commit_not_found hash) )
      | Error _ as e -> Lwt.return e
  end

  module Merge_result = struct
    type t = (unit, Irmin.Merge.conflict) result

    let encode : Raw.Builder.Store.MergeResult.t -> t -> unit =
     fun b t ->
      let open Raw.Builder.Store.MergeResult in
      match t with
      | Ok () -> ok_set b
      | Error (`Conflict msg) -> error_msg_set b msg

    let decode :
        Raw.Reader.Store.MergeResult.t -> (t, [ `Msg of string ]) result =
     fun str ->
      let open Raw.Reader.Store.MergeResult in
      match get str with
      | Ok -> Ok (Ok ())
      | ErrorMsg m -> Ok (Error (`Conflict m))
      | Undefined i -> errorf "Unknown MergeResult case with tag %i" i
  end

  module Push_result = struct
    type t =
      ( [ `Empty | `Head of Store.commit ],
        [ `Detached_head | `Msg of string ] )
      result

    let encode : t -> Raw.Builder.Sync.PushResult.t Lwt.t =
     fun t ->
      let open Raw.Builder.Sync.PushResult in
      let b = init_root () in
      match t with
      | Ok `Empty ->
          ok_empty_set b;
          Lwt.return b
      | Ok (`Head c) ->
          Store.Commit.hash c |> Hash.encode |> ok_head_set b;
          Lwt.return b
      | Error `Detached_head ->
          error_detached_head_set b;
          Lwt.return b
      | Error (`Msg m) ->
          error_msg_set b m;
          Lwt.return b
  end

  let encode_commit_info cm info =
    let module Info = Raw.Builder.Info in
    let i = Store.Commit.info cm in
    Info.author_set info (Irmin.Info.author i);
    Info.message_set info (Irmin.Info.message i);
    Info.date_set info (Irmin.Info.date i)
end

module Unit = struct
  type t = unit

  let encode, decode = codec_of_type Irmin.Type.unit
end
