type t = Raw.Client.Irmin.t Capnp_rpc_lwt.Capability.t

module type REMOTE = sig
  val remote : ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module type INFO = sig
  val info :
    ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type S = sig
  module Store : Irmin.S

  val local : Store.repo -> t
end

module type MAKER = functor (Store : Irmin.S) (_ : INFO) (_ : REMOTE) ->
  S with module Store = Store

module type Irmin_rpc = sig
  type nonrec t = t

  module type REMOTE = REMOTE

  module type INFO = INFO

  module type S = S

  module type MAKER = MAKER

  module Make : MAKER

  module Client = Client
end
