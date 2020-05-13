module Make
    (Store : Irmin.S)
    (Endpoint_codec : Irmin_rpc.Codec.SERIALISABLE
                        with type t = Store.Private.Sync.endpoint)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) : sig
  module Dns :
      module type of Dns_client_mirage.Make (Random) (Time) (Mclock) (Stack)

  module Server : sig
    include
      Irmin_rpc.Server.S
        with type repo = Store.repo
         and type store = Store.t
         and type commit = Store.commit
         and type hash = Store.hash

    val serve :
      secret_key:[< `PEM of string | `Ephemeral ] ->
      ?switch:Lwt_switch.t ->
      ?serve_tls:bool ->
      ?port:int ->
      Stack.t ->
      addr:string ->
      repo ->
      Uri.t Lwt.t
  end

  module Client : sig
    include
      Irmin_rpc.Client.S
        with type Store.tree = Store.tree
         and type Store.branch = Store.branch
         and type Store.key = Store.key
         and type Store.contents = Store.contents
         and type Store.hash = Store.hash

    val connect : Stack.t -> Uri.t -> t Lwt.t
  end
end
