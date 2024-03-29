open Common
module Client = Rpc.Client

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

(* This was printed when running the server example
 * Something like: "capnp://sha-256:QZVBfR2-8g6nfK7cRrD763Usn5Fg0j2muRXk62BhYKI@127.0.0.1:9998/yGlvMAKwxOw4B3lYe0g9XCuV4o5cp9BOQENvSvZNpjU" *)
let uri = Sys.argv.(1)

let info () =
  Client.Info.v ~author:"rpc-client-author" ~message:"rpc-client-message" 0L

let ( let* ) = Lwt.bind

let main =
  let* client = Uri.of_string uri |> Client.connect in
  let* repo = Client.repo client in
  let* master = Client.Store.master repo in
  let* tree = Client.Tree.empty repo in
  let* tree = Client.Tree.add tree [ "a" ] "1" in
  let* tree = Client.Tree.add tree [ "x"; "y"; "z" ] "999" in

  let* a = Client.Tree.find tree [ "a" ] in
  assert (Option.get a = "1");
  let* xyz = Client.Tree.find tree [ "x"; "y"; "z" ] in
  assert (Option.get xyz = "999");
  let* t = Client.Tree.find_tree tree [ "x"; "y" ] in
  let* z = Client.Tree.find (Option.get t) [ "z" ] in
  assert (Option.get z = "999");

  let* tree = Client.Tree.remove tree [ "a" ] in

  let* () = Client.Store.set_tree master ~info [] tree in
  let* a = Client.Store.find master [ "a" ] in
  assert (Result.get_ok a = None);
  let* xyz = Client.Store.get master [ "x"; "y"; "z" ] in
  assert (xyz = "999");
  Lwt.return ()

let () = Lwt_main.run main
