open Lwt.Infix

module Make
    (Store : Irmin.S)
    (Endpoint_codec : Irmin_rpc.Codec.SERIALISABLE
                        with type t = Store.Private.Sync.endpoint)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) =
struct
  module Capnp_rpc_mirage = Capnp_rpc_mirage.Make (Random) (Mclock) (Stack)
  module Dns = Capnp_rpc_mirage.Network.Dns

  module Server = struct
    module Rpc = Irmin_rpc.Make (Store) (Endpoint_codec)

    let serve ~secret_key ?switch ?serve_tls ?(port = 1111) stack ~addr repo =
      let dns = Dns.create stack in
      let net = Capnp_rpc_mirage.network ~dns stack in
      let public_address = `TCP (addr, port) in
      let config =
        Capnp_rpc_mirage.Vat_config.create ~secret_key ?serve_tls
          ~public_address (`TCP port)
      in
      let service_id = Capnp_rpc_mirage.Vat_config.derived_id config "main" in
      let restore =
        Capnp_rpc_net.Restorer.single service_id (Rpc.Server.local repo)
      in
      Capnp_rpc_mirage.serve ?switch net config ~restore >|= fun vat ->
      Capnp_rpc_mirage.Vat.sturdy_uri vat service_id

    include Rpc.Server
  end

  module Client = struct
    include Irmin_rpc.Client.Make (Store) (Endpoint_codec)

    let connect stack uri =
      let dns = Dns.create stack in
      let net = Capnp_rpc_mirage.network ~dns stack in
      let client_vat = Capnp_rpc_mirage.client_only_vat net in
      let sr = Capnp_rpc_mirage.Vat.import_exn client_vat uri in
      Capnp_rpc_lwt.Sturdy_ref.connect_exn sr
  end
end
