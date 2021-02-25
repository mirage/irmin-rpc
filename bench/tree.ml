open Bench_common

let ( >>= ) = Lwt.Infix.( >>= )

let ( >|= ) = Lwt.Infix.( >|= )

let ( let* ) x f = Lwt.bind x f

let ( let+ ) x f = Lwt.map f x

type key = string list [@@deriving yojson]

type hash = string [@@deriving yojson]

type message = string [@@deriving yojson]

type op =
  | Add of key * string
  | Remove of key
  | Find of key * bool
  | Mem of key * bool
  | Mem_tree of key * bool
  | Commit of hash * int64 * message * hash list
  | Checkout of hash
  | Copy of key * key
[@@deriving yojson]

module Parse_trace = struct
  let read_commits commits ncommits path =
    let json = Yojson.Safe.stream_from_file path in
    let rec aux index_op index_commit operations =
      if index_commit >= ncommits then index_commit
      else
        match Stream.next json with
        | exception Stream.Failure ->
            Fmt.epr
              "Only %d commits available in the trace file, proceeding...\n%!"
              index_commit;
            index_commit
        | op -> (
            match op_of_yojson op with
            | Ok (Commit _ as x) ->
                commits.(index_commit) <- List.rev (x :: operations);
                (aux [@tailcall]) (index_op + 1) (index_commit + 1) []
            | Ok x ->
                (aux [@tailcall]) (index_op + 1) index_commit (x :: operations)
            | Error s -> Fmt.failwith "error op_of_yosjon %s\n%!" s)
    in
    aux 0 0 []

  let populate_array ncommits path =
    let commits = Array.init ncommits (fun _ -> []) in
    let n = read_commits commits ncommits path in
    (commits, n)
end

module Generate_trees_from_trace
    (Store : Irmin_rpc.Client.S
               with type contents = string
                and type key = string list) =
struct
  type t = { mutable tx : Store.tx }

  type stats = {
    mutable adds : int;
    mutable removes : int;
    mutable finds : int;
    mutable mems : int;
    mutable mem_tree : int;
    mutable copies : int;
  }

  let stats =
    { adds = 0; removes = 0; finds = 0; mems = 0; mem_tree = 0; copies = 0 }

  let get_stats () = stats

  let error_find op k b n_op n_c =
    Fmt.failwith
      "Cannot reproduce operation %d of commit %d %s @[k = %a@] expected %b"
      n_op n_c op
      Fmt.(list ~sep:comma string)
      k b

  let add_operations t repo prev_commit operations n =
    Lwt_list.fold_left_s
      (fun (i, prev_commit) (operation : op) ->
        match operation with
        | Add (key, v) ->
            stats.adds <- succ stats.adds;
            let+ _ = Store.Tx.add t.tx key v in
            (i + 1, prev_commit)
        | Remove keys ->
            stats.removes <- succ stats.removes;
            let+ _ = Store.Tx.remove t.tx keys in
            (i + 1, prev_commit)
        | Find (keys, b) -> (
            stats.finds <- succ stats.finds;
            let* tree = Store.Tx.tree t.tx in
            Store.Tree.find tree keys >|= function
            | None when not b -> (i + 1, prev_commit)
            | Some _ when b -> (i + 1, prev_commit)
            | _ -> error_find "find" keys b i n)
        | Mem (keys, b) ->
            stats.mems <- succ stats.mems;
            let* tree = Store.Tx.tree t.tx in
            let+ b' = Store.Tree.mem tree keys in
            if b <> b' then error_find "mem" keys b i n;
            (i + 1, prev_commit)
        | Mem_tree (keys, b) ->
            stats.mem_tree <- succ stats.mem_tree;
            let* tree = Store.Tx.tree t.tx in
            let+ b' = Store.Tree.mem_tree tree keys in
            if b <> b' then error_find "mem_tree" keys b i n;
            (i + 1, prev_commit)
        | Checkout _ ->
            Store.Commit.of_hash repo (Option.get prev_commit) >>= fun commit ->
            (match commit with
            | None -> Fmt.failwith "prev commit not found"
            | Some commit ->
                Store.Commit.tree commit >>= fun tree ->
                let+ tx = Store.Tx.v repo tree in
                t.tx <- tx)
            >|= fun () -> (i + 1, prev_commit)
        | Copy (from, to_) -> (
            stats.copies <- succ stats.copies;

            let* tree = Store.Tx.tree t.tx in
            Store.Tree.find_tree tree from >>= function
            | None -> Lwt.return (i + 1, prev_commit)
            | Some sub_tree ->
                let+ _ = Store.Tx.add_tree t.tx to_ sub_tree in
                (i + 1, prev_commit))
        | Commit (_, date, message, _) ->
            (* in tezos commits call Tree.list first for the unshallow operation *)
            let* tree = Store.Tx.tree t.tx in
            let* _ = Store.Tree.list tree [] in
            let info () = Irmin.Info.v ~date ~author:"Tezos" message in
            let parents =
              match prev_commit with None -> [] | Some p -> [ p ]
            in
            Logs.info (fun l -> l "CCC");
            let* commit = Store.Commit.v repo ~info ~parents tree in
            (*let* () = Store.Tree.clear t.tree in*)
            let+ hash = Store.Commit.hash commit in
            (i + 1, Some hash))
      (0, prev_commit) operations

  let add_commits repo commits () =
    let* tree = Store.Tree.empty repo in
    let* tx = Store.Tx.v repo tree in
    let t = { tx } in
    let rec array_iter_lwt prev_commit i =
      if i >= Array.length commits then Lwt.return_unit
      else
        let operations = commits.(i) in
        let* _, prev_commit = add_operations t repo prev_commit operations i in
        array_iter_lwt prev_commit (i + 1)
    in
    array_iter_lwt None 0
end

type config = {
  ncommits : int;
  ncommits_trace : int;
  depth : int;
  nchain_trees : int;
  width : int;
  nlarge_trees : int;
  operations_file : string;
  root : string;
  quick : bool;
  uri : string;
}

module Benchmark = struct
  type result = { time : float; size : int }

  let run config f =
    let+ time, _ = with_timer f in
    let size = FSHelper.get_size config.root in
    { time; size }

  let pp_results fmt result =
    Format.fprintf fmt "Total time: %f@\nSize on disk: %d M" result.time
      result.size
end

module Hash = Irmin.Hash.SHA1

module Bench_suite (Conf : sig
  val entries : int

  val stable_hash : int
end) =
struct
  module Store =
    Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)
  module Rpc =
    Irmin_rpc_unix.Make
      (Store)
      (Irmin_rpc.Config.Remote.None (Store))
      (Irmin_rpc.Config.Pack.Make (Store))
  module Trees = Generate_trees (Rpc.Client)
  module Trees_trace = Generate_trees_from_trace (Rpc.Client)

  let init_commit repo =
    let* tree = Rpc.Client.Tree.empty repo in
    Logs.info (fun l -> l "init_commit");
    Rpc.Client.Commit.v repo ~info ~parents:[] tree

  let checkout_and_commit repo prev_commit f =
    let* commit = Rpc.Client.Commit.of_hash repo prev_commit in
    match commit with
    | None -> Lwt.fail_with "commit not found"
    | Some commit ->
        let* tree = Rpc.Client.Commit.tree commit in
        let* tx = Rpc.Client.Tree.Local.of_tree repo tree in
        let* tx = f tx in
        let* tree = Rpc.Client.Tree.Local.to_tree repo tx in
        Logs.info (fun l -> l "Checkout_and_commit");
        Rpc.Client.Commit.v repo ~info ~parents:[ prev_commit ] tree

  let add_commits repo ncommits f () : unit Lwt.t =
    let* c = init_commit repo in
    let rec aux c i =
      if i >= ncommits then Lwt.return c
      else
        let* hash = Rpc.Client.Commit.hash c in
        let* c' = checkout_and_commit repo hash f in
        aux c' (i + 1)
    in
    let+ _ = aux c 0 in
    ()

  let run ~mode config =
    reset_stats ();
    let* client = Rpc.Client.connect (Uri.of_string config.uri) in
    let* repo = Rpc.Client.repo client in
    let+ result =
      (match mode with
      | `Large ->
          Trees.add_large_trees config.width config.nlarge_trees
          |> add_commits repo config.ncommits
      | `Chains ->
          Trees.add_chain_trees config.depth config.nchain_trees
          |> add_commits repo config.ncommits)
      |> Benchmark.run config
    in
    (config, result)
end

module Bench_inodes_32 = Bench_suite (Conf)

module Bench_inodes_2 = Bench_suite (struct
  let entries = 2

  let stable_hash = 5
end)

type mode_elt = [ `Read_trace | `Chains | `Large ]

type suite_elt = {
  mode : mode_elt;
  speed : [ `Quick | `Slow ];
  inode_config : [ `Entries_32 | `Entries_2 ];
  run : config -> (config * Benchmark.result) Lwt.t;
}

let suite : suite_elt list =
  [
    (*{
        mode = `Chains;
        speed = `Quick;
        inode_config = `Entries_32;
        run = Bench_inodes_32.run ~mode:`Chains;
      };*)
    (*{
        mode = `Chains;
        speed = `Slow;
        inode_config = `Entries_2;
        run = Bench_inodes_2.run ~mode:`Chains;
      };*)
    {
      mode = `Large;
      speed = `Quick;
      inode_config = `Entries_32;
      run = Bench_inodes_32.run ~mode:`Large;
    };
    {
      mode = `Large;
      speed = `Slow;
      inode_config = `Entries_2;
      run = Bench_inodes_2.run ~mode:`Large;
    };
  ]

let pp_inode_config fmt = function
  | `Entries_2 -> Format.fprintf fmt "[2, 5]"
  | `Entries_32 -> Format.fprintf fmt "[32, 256]"

let pp_config b config fmt () =
  match b.mode with
  | `Read_trace ->
      let stats = Bench_inodes_32.Trees_trace.get_stats () in
      Format.fprintf fmt
        "Tezos_log mode on inode config %a: %d commits (%d adds; %d removes; \
         %d finds; %d mems; %d mem_tree; %d copies)"
        pp_inode_config b.inode_config config.ncommits_trace stats.adds
        stats.removes stats.finds stats.mems stats.mem_tree stats.copies
  | `Chains ->
      Format.fprintf fmt
        "Chain trees mode on inode config %a: %d commits, each consisting of \
         %d chains of depth %d"
        pp_inode_config b.inode_config config.ncommits config.nchain_trees
        config.depth
  | `Large ->
      Format.fprintf fmt
        "Large trees mode on inode config %a: %d commits, each consisting of \
         %d large trees of %d entries"
        pp_inode_config b.inode_config config.ncommits config.nlarge_trees
        config.width

let main ncommits ncommits_trace operations_file quick depth width nchain_trees
    nlarge_trees uri =
  let config =
    {
      ncommits;
      ncommits_trace;
      operations_file;
      root = "test-rpc";
      quick;
      depth;
      width;
      nchain_trees;
      nlarge_trees;
      uri;
    }
  in
  Printexc.record_backtrace true;
  Random.self_init ();
  FSHelper.rm_dir config.root;
  let suite =
      (* The suite contains two `Read_trace benchmarks. To prevent running both of
         them when `Quick is not set, we remove the first one (which is the head
         of the suite as well). *)
      if config.quick then List.filter (fun b -> b.speed = `Quick) suite
      else List.tl suite
    in
  let run_benchmarks () =
    Lwt_list.fold_left_s
      (fun (config, results) (b : suite_elt) ->
        let+ config, result = b.run config in
        (config, (b, result) :: results))
      (config, []) suite
  in
  let config, results = Lwt_main.run (run_benchmarks ()) in
  let pp_result fmt (b, result) =
    Format.fprintf fmt "Configuration:@\n @[%a@]@\n@\nResults:@\n @[%a@]@\n"
      (pp_config b config) () Benchmark.pp_results result
  in
  Fmt.pr "%a@." Fmt.(list ~sep:(any "@\n@\n") pp_result) results

open Cmdliner

let quick =
  let doc = Arg.info ~doc:"Run the quick benchmarks" [ "quick" ] in
  Arg.(value @@ flag doc)

let ncommits =
  let doc =
    Arg.info ~doc:"Number of commits for the large and chain modes."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt int 2 doc)

let ncommits_trace =
  let doc =
    Arg.info ~doc:"Number of commits to read from trace." [ "ncommits_trace" ]
  in
  Arg.(value @@ opt int 13315 doc)

let depth =
  let doc =
    Arg.info ~doc:"Depth of a commit's tree in chains-mode." [ "d"; "depth" ]
  in
  Arg.(value @@ opt int 1000 doc)

let nchain_trees =
  let doc =
    Arg.info ~doc:"Number of chain trees per commit in chains-mode."
      [ "c"; "nchain" ]
  in
  Arg.(value @@ opt int 1 doc)

let width =
  let doc =
    Arg.info ~doc:"Width of a commit's tree in large-mode." [ "w"; "width" ]
  in
  Arg.(value @@ opt int 1000000 doc)

let nlarge_trees =
  let doc =
    Arg.info ~doc:"Number of large trees per commit in large-mode."
      [ "l"; "nlarge" ]
  in
  Arg.(value @@ opt int 1 doc)

let operations_file =
  let doc =
    Arg.info ~doc:"Compressed file containing the tree operations."
      [ "f"; "file" ]
  in
  Arg.(value @@ opt string "bench/irmin-pack/tezos_log.tar.gz" doc)

let uri =
  let doc = Arg.info ~doc:"Cap'n'proto URL" [] in
  Arg.(value @@ pos 0 string "" doc)

let main_term =
  Term.(
    const main
    $ ncommits
    $ ncommits_trace
    $ operations_file
    $ quick
    $ depth
    $ width
    $ nchain_trees
    $ nlarge_trees
    $ uri)

let () =
  let info = Term.info "Benchmarks for tree operations" in
  Term.exit @@ Term.eval (main_term, info)
