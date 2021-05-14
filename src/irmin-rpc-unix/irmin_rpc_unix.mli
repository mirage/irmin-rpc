module Remote : sig
  module Git :
    Irmin_rpc.Codec.SERIALISABLE with type t = Mimic.ctx * Smart_git.Endpoint.t
end

module Make
    (Store : Irmin.S)
    (Remote : Irmin_rpc.Config.REMOTE with type t = Store.Private.Remote.endpoint)
    (Pack : Irmin_rpc.Config.PACK with type repo = Store.repo) : sig
  module Server : sig
    type t

    val uri : t -> Uri.t

    val serve :
      ?backlog:int ->
      ?switch:Lwt_switch.t ->
      ?secure:bool ->
      secret_key:[< `File of string | `PEM of string | `Ephemeral ] ->
      Capnp_rpc_unix.Network.Location.t ->
      Store.repo ->
      t Lwt.t
    (** Initialise an Irmin RPC server hosted at the given network location
        serving data from the given repository.

        - Backlog is the maximal number of pending requests (passed to
          {!Unix.listen}).

        - If [secure] is true (default), the server performs a TLS handshake
          using the provided [secret_key]. Otherwise, the server will accept any
          unencrypted incoming connection.

        - Turning off the supplied Lwt switch will terminate the server
          asynchronously. *)
  end

  module Client : sig
    include
      Irmin_rpc.Client.S
        with type key = Store.key
         and type step = Store.Key.step
         and type hash = Store.hash
         and type branch = Store.branch
         and type contents = Store.contents

    val connect : Uri.t -> t Lwt.t
  end
end
