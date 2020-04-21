module Make (Store : Irmin.S) (Remote : Irmin_rpc.REMOTE) : sig
  module Rpc : Irmin_rpc.S with module Store = Store

  module Server : sig
    type t

    val uri : t -> Uri.t

    val create :
      ?backlog:int ->
      secret_key:[< `File of string | `PEM of string | `Ephemeral ] ->
      ?serve_tls:bool ->
      Capnp_rpc_unix.Network.Location.t ->
      Store.repo ->
      t Lwt.t

    val run : t -> unit Lwt.t
  end

  module Client : sig
    type t = Irmin_rpc.t

    val connect : Uri.t -> t Lwt.t

    include Irmin_rpc.CLIENT with module Store = Store
  end
end
