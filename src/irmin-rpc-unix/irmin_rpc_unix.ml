open Lwt.Infix

module Git_unix_endpoint_codec = struct
  type t = Git_unix.endpoint

  let encode Git_unix.{ uri; headers } =
    let open Sexplib0.Sexp_conv in
    (Uri.to_string uri, headers)
    |> sexp_of_pair sexp_of_string Cohttp.Header.sexp_of_t
    |> Sexplib0.Sexp.to_string

  let decode str =
    let open Sexplib0.Sexp_conv in
    let uri_string, headers =
      str
      |> Sexplib.Sexp.of_string
      |> pair_of_sexp string_of_sexp Cohttp.Header.t_of_sexp
    in
    Ok Git_unix.{ uri = Uri.of_string uri_string; headers }
end

module Make
    (Store : Irmin.S)
    (Endpoint_codec : Irmin_rpc.Codec.SERIALISABLE
                        with type t = Store.Private.Sync.endpoint) =
struct
  module Server = struct
    module Api = Irmin_rpc.Server.Make (Store) (Endpoint_codec)

    type t = { uri : Uri.t }

    let uri { uri; _ } = uri

    let serve ?backlog ?switch ?secure:serve_tls ~secret_key addr repo =
      let config =
        Capnp_rpc_unix.Vat_config.create ?backlog ~secret_key ?serve_tls addr
      in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
      let restore =
        Capnp_rpc_net.Restorer.single service_id (Api.make_irmin repo)
      in
      Capnp_rpc_unix.serve ?switch ~restore config >|= fun vat ->
      { uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id }
  end

  module Client = struct
    include Irmin_rpc.Client.Make (Store) (Endpoint_codec)

    let connect uri =
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
      Capnp_rpc_lwt.Sturdy_ref.connect_exn sr
  end
end
