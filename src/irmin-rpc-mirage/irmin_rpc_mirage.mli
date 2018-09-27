module Make
    (Store: Irmin.S)
    (Clock: Mirage_clock_lwt.PCLOCK)
    (Time: Mirage_time_lwt.S)
    (Stack : Mirage_stack_lwt.V4)
: sig
  module Dns: Dns_resolver_mirage.S with type stack = Stack.t
  module Server(C: sig val clock: Clock.t end): sig
    module Rpc: Irmin_rpc.S with module Store = Store

    type t
    val uri: t -> Uri.t

    val create:
      secret_key: [< `PEM of string | `Ephemeral] ->
      ?serve_tls: bool ->
      ?port:int ->
      Stack.t ->
      addr:string ->
      Store.repo -> t Lwt.t

    val run: t -> 'a Lwt.t
  end

  module Client: sig
    type t = Irmin_rpc.t
    val connect: Stack.t -> Uri.t -> t Lwt.t

    include Irmin_rpc.CLIENT with module Store = Store
  end
end
