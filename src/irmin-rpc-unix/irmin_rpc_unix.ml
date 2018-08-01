open Lwt.Infix

module Make(Store: Irmin.KV) = struct

  module Rpc = Irmin_rpc.Make(Store)(struct
    let info = Irmin_unix.info
  end)

  module Server = struct
    let start ?backlog ~secret_key ?serve_tls addr repo =
      let config = Capnp_rpc_unix.Vat_config.create ?backlog ~secret_key ?serve_tls addr in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
      let restore = Capnp_rpc_lwt.Restorer.single service_id (Rpc.local repo) in
      Capnp_rpc_unix.serve config ~restore >|= fun vat ->
      Capnp_rpc_unix.Vat.sturdy_uri vat service_id

  end

  module Client = struct

  end
end
