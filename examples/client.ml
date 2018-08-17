open Lwt.Infix

module Store = Irmin_mem.KV(Irmin.Contents.String)
module Rpc = Irmin_rpc_unix.Make(Store)

(* This was printed when running the server example above *)
let uri = "capnp://sha-256:YIhQi5oAx0XAUJc7XnbhbNooKDds0LV9zbtsepd3X6A@127.0.0.1:9999/WUNVqiE4hrUdV6GvTvnKq6yg-8xVvJmILcLlwPUVldo"

let main =
    Rpc.Client.connect (Uri.of_string uri) >>= fun client ->
    Rpc.Client.set client ["abc"] "123" >>= fun _ ->
    Rpc.Client.get client ["abc"] >|= function
    | Ok res -> assert (res = "123"); print_endline res
    | Error _ -> print_endline "Error encountered"

let () = Lwt_main.run main
