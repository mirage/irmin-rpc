type t = [ `Irmin_b2b5cb4fd15c7d5a ] Capnp_rpc_lwt.Capability.t

module type S = sig
  module Store: Irmin.KV

  val local: Store.repo -> t

  module Client: sig
    val get: t -> Store.key -> (Store.contents, [`Msg of string]) result Lwt.t
    val set: t -> Store.key -> Store.contents -> bool Lwt.t
    val remove: t -> Store.key -> unit Lwt.t
  end
end

module Make(Store: Irmin.KV)(Info: sig
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end): S with module Store = Store
