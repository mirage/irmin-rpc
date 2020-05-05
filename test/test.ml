(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
module Store = Irmin_unix.Git.Mem.KV (Irmin.Contents.String)
module Rpc =
  Irmin_rpc_unix.Make (Store) (Irmin_rpc_unix.Git_unix_endpoint_codec)

let _ = Logs.set_reporter (Logs.format_reporter ())

let _ = Unix.system "rm -rf ./db"

let cfg = Irmin_git.config "db"

let hash_s = Irmin.Type.to_string Store.Hash.t

let commit = ref None

let author, message = ("rpc-client-author", "rpc-client-message")

let test_snapshot_no_head t _switch () =
  Lwt.catch
    (fun () ->
      Rpc.Client.snapshot t >|= fun _ -> Alcotest.fail "Expected exception")
    (fun _ -> Lwt.return_unit)

let test_set t _switch () =
  Rpc.Client.set t [ "a"; "b"; "c" ] "123" ~message:"abc=>123" ~author:"test"
  >>= fun _ ->
  Rpc.Client.find t [ "a"; "b"; "c" ] >|= function
  | Some res -> Alcotest.(check string) "get a/b/c" res "123"
  | _ -> Alcotest.fail "a/b/c not set when it is expected to be set"

let test_find_not_found t _switch () =
  Rpc.Client.find t [ "abc" ] >>= function
  | None -> Lwt.return_unit
  | _ -> Alcotest.fail "abc set when it is expected to be unset"

let test_remove t _switch () =
  Rpc.Client.snapshot t >>= fun snapshot ->
  commit := snapshot;
  match snapshot with
  | Some snapshot ->
      Rpc.Client.remove ~author ~message t [ "abc" ] >>= fun hash ->
      Alcotest.(check string)
        "Check snapshot hash" (hash_s hash) (hash_s snapshot);
      Rpc.Client.remove ~author ~message t [ "a"; "b"; "c" ] >>= fun hash ->
      Alcotest.(check (neg string))
        "Check snapshot hash after modification" (hash_s hash) (hash_s snapshot);
      Lwt.return_unit
  | None -> Alcotest.fail "Expected commit"

let test_revert t _switch () =
  match !commit with
  | Some commit ->
      Rpc.Client.revert t commit >>= fun b ->
      Alcotest.(check bool) "revert" b true;
      Rpc.Client.get t [ "a"; "b"; "c" ] >|= fun res ->
      Alcotest.(check string) "revert value" res "123"
  | None -> Alcotest.fail "Revert commit hash is not defined"

let test_set_tree t _switch () =
  let tree = Store.Tree.empty in
  Store.Tree.add tree [ "foo"; "a" ] "1" >>= fun tree ->
  Store.Tree.add tree [ "foo"; "b" ] "2" >>= fun tree ->
  Store.Tree.add tree [ "foot"; "c" ] "3" >>= fun tree ->
  Rpc.Client.Tree.set t ~author:"Testing" ~message:"Hello" [] tree
  >>= fun hash ->
  Rpc.Client.Tree.get t [ "foo" ] >>= fun tree' ->
  Store.Tree.get_tree tree [ "foo" ] >>= fun tree ->
  Store.Tree.diff tree tree' >>= fun diff ->
  Alcotest.(check int) "tree diff" 0 (List.length diff);
  Rpc.Client.Commit.info t hash >|= function
  | Some info ->
      Alcotest.(check string) "info author" "Testing" (Irmin.Info.author info);
      Alcotest.(check string) "info message" "\nHello" (Irmin.Info.message info)
  | None -> Alcotest.fail "Expected commit info"

(* TODO: look into why this fails when run with [opam install --build-test] but
   not [dune runtest] *)
let test_pull t _switch () =
  Uri.of_string "git://github.com/mirage/irmin-rpc.git"
  |> Git_unix.endpoint
  |> Rpc.Client.Sync.pull t ~author ~message
  >>= function
  | Ok _hash ->
      Rpc.Client.get t [ "README.md" ] >|= fun readme ->
      let f = open_in "../../../README.md" in
      let n = in_channel_length f in
      let readme' = really_input_string f n in
      close_in f;
      Alcotest.(check string) "readme" readme readme'
  | Error (`Msg e) -> Alcotest.fail e

let test_merge t _switch () =
  Rpc.Client.merge t ~author ~message ~branch:"testing" "master" >>= fun _ ->
  Rpc.Client.set t ~author ~message ~branch:"testing" [ "README.md" ] "merged!"
  >>= fun _ ->
  Rpc.Client.merge t ~author ~message "testing" >>= function
  | Ok _hash ->
      Rpc.Client.get t [ "README.md" ] >|= fun readme ->
      Alcotest.(check string) "readme - merge" readme "merged!"
  | Error (`Msg msg) -> Alcotest.fail msg

let test_create_remove_branch t _switch () =
  Rpc.Client.snapshot t >>= fun head ->
  match head with
  | Some head ->
      Rpc.Client.Branch.create t "aaa" head >>= fun _ ->
      Rpc.Client.Branch.list t >|= fun l ->
      let l = List.mem "aaa" l in
      Alcotest.(check bool) "branches contains 'aaa'" l true
  | None -> Alcotest.fail "No head commit found"

let local t =
  [
    Alcotest_lwt.test_case "snapshot (no head)" `Quick
    @@ test_snapshot_no_head t;
    Alcotest_lwt.test_case "set" `Quick @@ test_set t;
    Alcotest_lwt.test_case "get" `Quick @@ test_find_not_found t;
    Alcotest_lwt.test_case "del" `Quick @@ test_remove t;
    Alcotest_lwt.test_case "revert" `Quick @@ test_revert t;
    Alcotest_lwt.test_case "get_tree/set_tree" `Quick @@ test_set_tree t;
    Alcotest_lwt.test_case "pull" `Quick @@ test_pull t;
    Alcotest_lwt.test_case "merge" `Quick @@ test_merge t;
    Alcotest_lwt.test_case "create/remove branch" `Quick
    @@ test_create_remove_branch t;
  ]

let main =
  Store.Repo.v cfg >>= fun repo ->
  Alcotest_lwt.run "RPC" [ ("Local", local (Rpc.Rpc.local repo)) ]

let () = Lwt_main.run main

(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
