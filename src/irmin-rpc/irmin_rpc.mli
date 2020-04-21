type t = [ `Irmin_b2b5cb4fd15c7d5a ] Capnp_rpc_lwt.Capability.t

exception Error_message of string

module type CLIENT = sig
  module Store : Irmin.S

  val get : t -> ?branch:Store.branch -> Store.key -> Store.contents Lwt.t

  val find :
    t -> ?branch:Store.branch -> Store.key -> Store.contents option Lwt.t

  val set :
    t ->
    ?branch:Store.branch ->
    ?author:string ->
    ?message:string ->
    Store.key ->
    Store.contents ->
    Store.Hash.t Lwt.t

  val remove :
    t ->
    ?branch:Store.branch ->
    ?author:string ->
    ?message:string ->
    Store.key ->
    Store.Hash.t Lwt.t

  val merge :
    t ->
    ?branch:Store.branch ->
    ?author:string ->
    ?message:string ->
    Store.branch ->
    (Store.Hash.t, [ `Msg of string ]) result Lwt.t

  val snapshot : ?branch:Store.branch -> t -> Store.Hash.t option Lwt.t

  val revert : t -> ?branch:Store.branch -> Store.Hash.t -> bool Lwt.t

  module Tree : sig
    val set :
      t ->
      ?branch:Store.branch ->
      ?author:string ->
      ?message:string ->
      Store.key ->
      Store.tree ->
      Store.Hash.t Lwt.t

    val find : t -> ?branch:Store.branch -> Store.key -> Store.tree option Lwt.t

    val get : t -> ?branch:Store.branch -> Store.key -> Store.tree Lwt.t
  end

  module Sync : sig
    val clone :
      t ->
      ?branch:Store.branch ->
      string ->
      (Store.Hash.t, [ `Msg of string ]) result Lwt.t

    val pull :
      t ->
      ?branch:Store.branch ->
      ?author:string ->
      ?message:string ->
      string ->
      (Store.Hash.t, [ `Msg of string ]) result Lwt.t

    val push :
      t ->
      ?branch:Store.branch ->
      string ->
      (unit, [ `Msg of string ]) result Lwt.t
  end

  module Commit : sig
    val info : t -> Store.Hash.t -> Irmin.Info.t option Lwt.t

    val history : t -> Store.Hash.t -> Store.Hash.t list Lwt.t
  end

  module Branch : sig
    val list : t -> Store.branch list Lwt.t

    val remove : t -> Store.branch -> unit Lwt.t

    val create : t -> Store.branch -> Store.Hash.t -> unit Lwt.t
  end
end

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

module Make (Store : Irmin.S) (Info : INFO) (Remote : REMOTE) :
  S with module Store = Store

module Client (Store : Irmin.S) : CLIENT with module Store = Store
