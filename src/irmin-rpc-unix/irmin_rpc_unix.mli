module Git_unix_endpoint_codec :
  Irmin_rpc.Codec.SERIALISABLE with type t = Git_unix.endpoint

module Make
    (Store : Irmin.S)
    (Endpoint_codec : Irmin_rpc.Codec.SERIALISABLE
                        with type t = Store.Private.Sync.endpoint) : sig
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
    include Irmin_rpc.Client.S with module Store = Store

    val connect : Uri.t -> t Lwt.t
  end
end
