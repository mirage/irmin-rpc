type t = [`Irmin_b2b5cb4fd15c7d5a] Capnp_rpc_lwt.Capability.t

exception Error_message of string

module type CLIENT = sig
  module Store : Irmin.S

  val get : t -> ?branch:Store.branch -> Store.key -> Store.contents Lwt.t

  val get_tree : t -> ?branch:Store.branch -> Store.key -> Store.tree Lwt.t

  val find :
    t -> ?branch:Store.branch -> Store.key -> Store.contents option Lwt.t

  val find_tree :
    t -> ?branch:Store.branch -> Store.key -> Store.tree option Lwt.t

  val set :
       t
    -> ?branch:Store.branch
    -> ?author:string
    -> ?message:string
    -> Store.key
    -> Store.contents
    -> Store.Commit.hash Lwt.t

  val set_tree :
       t
    -> ?branch:Store.branch
    -> ?author:string
    -> ?message:string
    -> Store.key
    -> Store.tree
    -> Store.Commit.hash Lwt.t

  val remove :
       t
    -> ?branch:Store.branch
    -> ?author:string
    -> ?message:string
    -> Store.key
    -> Store.Commit.hash Lwt.t

  val clone :
       t
    -> ?branch:Store.branch
    -> string
    -> (Store.Commit.hash, [`Msg of string]) result Lwt.t

  val pull :
       t
    -> ?branch:Store.branch
    -> ?author:string
    -> ?message:string
    -> string
    -> (Store.Commit.hash, [`Msg of string]) result Lwt.t

  val push :
       t
    -> ?branch:Store.branch
    -> string
    -> (unit, [`Msg of string]) result Lwt.t

  val merge :
       t
    -> ?branch:Store.branch
    -> ?author:string
    -> ?message:string
    -> Store.branch
    -> (Store.Commit.hash, [`Msg of string]) result Lwt.t

  val commit_info : t -> Store.Commit.Hash.t -> Irmin.Info.t option Lwt.t

  val snapshot : ?branch:Store.branch -> t -> Store.Commit.Hash.t option Lwt.t

  val revert : t -> ?branch:Store.branch -> Store.Commit.Hash.t -> bool Lwt.t

  val branches : t -> Store.branch list Lwt.t

  val commit_history :
    t -> Store.Commit.Hash.t -> Store.Commit.Hash.t list Lwt.t

  val remove_branch : t -> Store.branch -> unit Lwt.t
  val create_branch : t -> Store.branch -> Store.Commit.hash -> unit Lwt.t
end

module type REMOTE = sig
  val remote: ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module type INFO = sig
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type S = sig
  module Store : Irmin.S
  val local : Store.repo -> t
end

module Make (Store : Irmin.S)(Info: INFO)(Remote: REMOTE): S with module Store = Store
module Client(Store : Irmin.S): CLIENT with module Store = Store
