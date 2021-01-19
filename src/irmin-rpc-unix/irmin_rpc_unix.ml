open Lwt.Infix

module Remote = struct
  module Git = struct
    type t = Mimic.ctx * Smart_git.Endpoint.t

    let encode (_, endpoint) = Fmt.to_to_string Smart_git.Endpoint.pp endpoint

    let decode str =
      Result.map (fun x -> (Mimic.empty, x)) @@ Smart_git.Endpoint.of_string str
  end
end

module Make
    (Store : Irmin.S)
    (Remote : Irmin_rpc.Config.REMOTE with type t = Store.Private.Sync.endpoint)
    (Pack : Irmin_rpc.Config.PACK with type repo = Store.repo) =
struct
  module Server = struct
    module Api = Irmin_rpc.Server.Make (Store) (Remote) (Pack)

    type t = { uri : Uri.t }

    let uri { uri; _ } = uri

    let serve ?backlog ?switch ?secure:serve_tls ~secret_key addr repo =
      let config =
        Capnp_rpc_unix.Vat_config.create ?backlog ~secret_key ?serve_tls addr
      in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
      let restore = Capnp_rpc_net.Restorer.single service_id (Api.local repo) in
      Capnp_rpc_unix.serve ?switch ~restore config >|= fun vat ->
      { uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id }
  end

  module Client = struct
    include Irmin_rpc.Client.Make (Store) (Remote) (Pack)

    let connect uri =
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
      Capnp_rpc_lwt.Sturdy_ref.connect_exn sr
  end
end
