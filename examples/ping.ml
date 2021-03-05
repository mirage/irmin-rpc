open Lwt.Syntax
open Common
module Client = Rpc.Client

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

(* This was printed when running the server example
 * Something like: "capnp://sha-256:QZVBfR2-8g6nfK7cRrD763Usn5Fg0j2muRXk62BhYKI@127.0.0.1:9998/yGlvMAKwxOw4B3lYe0g9XCuV4o5cp9BOQENvSvZNpjU" *)
let uri = Sys.argv.(1)

let ( let* ) = Lwt.bind

let main =
  let* client = Uri.of_string uri |> Client.connect in
  let+ x = Client.ping client in
  Result.get_ok x

let () = Lwt_main.run main
