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

    val run: t -> 'a Lwt.t
  end

  module Client: sig
    type t = Irmin_rpc.t

    val connect: Uri.t -> t Lwt.t
    val get: t -> ?branch:string -> Store.key -> (Store.contents, [`Msg of string]) result Lwt.t
    val set: t -> ?branch:string -> Store.key -> Store.contents -> bool Lwt.t
    val remove: t -> ?branch:string -> Store.key -> unit Lwt.t
  end
end
