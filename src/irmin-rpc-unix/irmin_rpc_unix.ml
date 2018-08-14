open Lwt.Infix

module Make(Store: Irmin.KV) = struct

  module Rpc = Irmin_rpc.Make(Store)(struct
    let info = Irmin_unix.info
  end)

  module Server = struct
    type t = {
      uri: Uri.t
    }

    let uri {uri;_} = uri

    let create ?backlog ~secret_key ?serve_tls addr repo =
      let config = Capnp_rpc_unix.Vat_config.create ?backlog ~secret_key ?serve_tls addr in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
      let restore = Capnp_rpc_lwt.Restorer.single service_id (Rpc.local repo) in
      Capnp_rpc_unix.serve config ~restore >|= fun vat ->
      {uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id}

    let run _t = Lwt.wait ()
  end

  module Client = struct
    type t = Irmin_rpc.t

    let connect uri =
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
      Capnp_rpc_lwt.Sturdy_ref.connect_exn sr

    let get t ?branch (key: Store.key) =
      Rpc.Client.get t ?branch key

    let set t ?branch key value =
      Rpc.Client.set t ?branch key value

    let remove t ?branch key =
      Rpc.Client.remove t ?branch key
  end
end
