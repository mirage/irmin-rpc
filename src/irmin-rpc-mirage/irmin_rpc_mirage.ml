open Lwt.Infix

module Make(Store: Irmin.S)(Clock: Mirage_clock_lwt.PCLOCK)(Time: Mirage_time_lwt.S)(Stack : Mirage_stack_lwt.V4) = struct
  module Dns = Dns_resolver_mirage.Make(Time)(Stack)

  module Capnp_rpc_mirage = Capnp_rpc_mirage.Make(Stack)(Dns)

  module Server(C: sig val clock: Clock.t end) = struct

    module Rpc = Irmin_rpc.Make(Store)(struct
      let info ?(author = "irmin-rpc") =
        let module Info =
          Irmin_mirage.Info(struct
            let name = author
          end)(Clock)
        in
        Info.f C.clock
    end)

    type t = {
      uri: Uri.t
    }

    let uri {uri;_} = uri

    let create ~secret_key ?serve_tls ?(port = 1111) stack ~addr repo =
      let dns = Dns.create stack in
      let net = Capnp_rpc_mirage.network ~dns stack in
      let public_address = `TCP (addr, port) in
      let config = Capnp_rpc_mirage.Vat_config.create ~secret_key ?serve_tls ~public_address (`TCP port) in
      let service_id = Capnp_rpc_mirage.Vat_config.derived_id config "main" in
      let restore = Capnp_rpc_lwt.Restorer.single service_id (Rpc.local repo) in
      Capnp_rpc_mirage.serve net config ~restore >|= fun vat ->
      {uri = Capnp_rpc_mirage.Vat.sturdy_uri vat service_id}

    let run _t = fst @@ Lwt.wait ()
  end

  module Client(C: sig val clock: Clock.t end) = struct
    type t = Irmin_rpc.t

    module Rpc = Irmin_rpc.Make(Store)(struct
      let info ?(author = "irmin-rpc") =
        let module Info =
          Irmin_mirage.Info(struct
            let name = author
          end)(Clock)
        in
        Info.f C.clock
    end)

    include Rpc.Client

    let connect stack uri =
      let dns = Dns.create stack in
      let net = Capnp_rpc_mirage.network ~dns stack in
      let client_vat = Capnp_rpc_mirage.client_only_vat net in
      let sr = Capnp_rpc_mirage.Vat.import_exn client_vat uri in
      Capnp_rpc_lwt.Sturdy_ref.connect_exn sr
  end
end
