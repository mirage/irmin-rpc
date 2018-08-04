module Make(Store: Irmin.KV): sig
  module Rpc: Irmin_rpc.S with module Store = Store
  module Server: sig
    type t

    val uri: t -> Uri.t

    val create:
      ?backlog: int ->
      secret_key: [< `File of string | `PEM of string | `Ephemeral] ->
      ?serve_tls: bool ->
      Capnp_rpc_unix.Network.Location.t ->
      Store.repo -> t Lwt.t

    val run: t -> 'a Lwt.t * 'a Lwt.u
  end

  module Client: sig
    (*type 'a t = 'a Capnp_rpc_lwt.Capability.t

    val connect: Uri.t -> 'a t Lwt.t*)
  end
end
