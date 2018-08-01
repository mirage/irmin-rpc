module Make(Store: Irmin.KV): sig
  module Server: sig
    val start:
      ?backlog: int ->
      secret_key: [< `File of string | `PEM of string | `Ephemeral] ->
      ?serve_tls: bool ->
      Capnp_rpc_unix.Network.Location.t ->
      Store.repo -> Uri.t Lwt.t
  end
end
