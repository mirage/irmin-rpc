module Make
    (Store: Irmin.S)
    (Clock: Mirage_clock_lwt.PCLOCK)
    (Network: Mirage_net_lwt.S)
    (Stack : Mirage_stack_lwt.V4)
    (Dns : Dns_resolver_mirage.S)
: sig
  module Server(C: sig val clock: Clock.t end): sig
    module Rpc: Irmin_rpc.S with module Store = Store

    type t
    val uri: t -> Uri.t

    val create:
      secret_key: [< `PEM of string | `Ephemeral] ->
      ?serve_tls: bool ->
      ?port:int ->
      Stack.t ->
      Dns.t ->
      addr:string ->
      Store.repo -> t Lwt.t

    val run: t -> 'a Lwt.t
  end

  module Client(C: sig val clock: Clock.t end): sig
    module Rpc: Irmin_rpc.S with module Store = Store
    type t = Irmin_rpc.t
    val connect: Stack.t -> Dns.t -> Uri.t -> t Lwt.t

    include Irmin_rpc.CLIENT with module Store = Store
  end
end
