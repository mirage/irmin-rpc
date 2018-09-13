(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module Store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Rpc = Irmin_rpc_unix.Make(Store)

let cfg = Irmin_mem.config ()

let hash_s = Fmt.to_to_string Store.Commit.Hash.pp

let test_set t _switch () =
  Rpc.Client.set t ["a"; "b"; "c"] "123" ~message:"abc=>123" ~author:"test" >>= fun _ ->
  Rpc.Client.get t ["a"; "b"; "c"] >|= function
    | Ok (Some res) -> Alcotest.(check string) "get a/b/c" res "123"
    | _ -> Alcotest.fail "a/b/c not set when it is expected to be set"

let test_get_not_found t _switch () =
  (Rpc.Client.get t ["abc"] >>= function
    | Ok None -> Lwt.return_unit
    | Ok _ -> Alcotest.fail "abc set when it is expected to be unset"
    | Error (`Msg m) -> Alcotest.fail m)

let test_remove t _switch () =
  Rpc.Client.snapshot t >>= (function Ok hash -> Lwt.return hash | Error (`Msg msg) -> failwith msg)  >>= fun snapshot ->
  (Rpc.Client.remove t ["abc"] >>= fun hash ->
  Alcotest.(check string) "Check snapshot hash" (hash_s hash) (hash_s snapshot);
  Rpc.Client.remove t ["a"; "b"; "c"] >>= fun hash ->
  Alcotest.(check (neg string)) "Check snapshot hash after modification" (hash_s hash) (hash_s snapshot);
  Lwt.return_unit)


let local t = [
  Alcotest_lwt.test_case "set" `Quick @@ test_set t;
  Alcotest_lwt.test_case "get" `Quick @@ test_get_not_found t;
  Alcotest_lwt.test_case "del" `Quick @@ test_remove t;
]

let main =
  Store.Repo.v cfg >|= fun repo ->
  Alcotest.run "RPC" [
    "Local", local (Rpc.Rpc.local repo);
  ]

let () = Lwt_main.run main


(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
