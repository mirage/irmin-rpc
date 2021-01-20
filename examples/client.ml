open Lwt.Infix
open Common
module Client = Rpc.Client

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

(* This was printed when running the server example
 * Something like: "capnp://sha-256:QZVBfR2-8g6nfK7cRrD763Usn5Fg0j2muRXk62BhYKI@127.0.0.1:9998/yGlvMAKwxOw4B3lYe0g9XCuV4o5cp9BOQENvSvZNpjU" *)
let uri = Sys.argv.(1)

let info () =
  Irmin_unix.info ~author:"rpc-client-author" "rpc-client-message" ()

let ( let* ) = Lwt.bind

let main =
  let* client = Uri.of_string uri |> Client.connect in
  let* repo = Client.repo client in
  let* master = Client.Store.master repo in
  Client.ping client >>= fun x ->
  let () = Result.get_ok x in
  let* () = Client.Store.set ~info master [ "abc" ] "123" in
  let* res = Client.Store.get master [ "abc" ] in
  assert (res = "123");
  print_endline res;
  let () = Lwt.ignore_result @@ Client.Store.find_tree master [] in
  (*let* l = Store.Tree.list tree [] in
    assert (List.length l = 1);*)
  (*let* pack = Client.Store.pack master in
    let* check = Client.Store.Pack.integrity_check (Option.get pack) in
    assert (Result.is_ok check);*)
  Lwt.return ()

let () = Lwt_main.run main
