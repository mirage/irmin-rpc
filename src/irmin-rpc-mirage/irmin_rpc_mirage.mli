module Make
    (Store : Irmin.S)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) : sig
  module Dns : module type of Dns_client_mirage.Make (Random) (Mclock) (Stack)

  module Server : sig
    module Rpc : Irmin_rpc.S with module Store = Store

    type t

    val uri : t -> Uri.t

    val create :
      secret_key:[< `PEM of string | `Ephemeral ] ->
      ?serve_tls:bool ->
      ?port:int ->
      Stack.t ->
      addr:string ->
      Store.repo ->
      t Lwt.t

    val run : t -> 'a Lwt.t
  end

  module Client : sig
    type t = Irmin_rpc.t

    val connect : Stack.t -> Uri.t -> t Lwt.t

    include Irmin_rpc.CLIENT with module Store = Store
  end
end
