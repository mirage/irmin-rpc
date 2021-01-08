open Lwt.Infix
module Underlying_store = Irmin_unix.Git.Mem.KV (Irmin.Contents.String)
module Rpc =
  Irmin_rpc_unix.Make
    (Underlying_store)
    (Irmin_rpc_unix.Git_unix_endpoint_codec)
module Store = Rpc.Client.Store

(* This was printed when running the server example
 * Something like: "capnp://sha-256:QZVBfR2-8g6nfK7cRrD763Usn5Fg0j2muRXk62BhYKI@127.0.0.1:9998/yGlvMAKwxOw4B3lYe0g9XCuV4o5cp9BOQENvSvZNpjU" *)
let uri = Sys.argv.(1)

let info () =
  Irmin_unix.info ~author:"rpc-client-author" "rpc-client-message" ()

let ( let* ) = Lwt.bind

let main =
  let* repo = Uri.of_string uri |> Rpc.Client.connect >>= Rpc.Client.repo in
  let* master = Store.master repo in
  let* () = Store.set ~info master [ "abc" ] "123" in
  let* res = Store.get master [ "abc" ] in
  assert (res = "123");
  print_endline res;
  Lwt.return ()

let () = Lwt_main.run main
