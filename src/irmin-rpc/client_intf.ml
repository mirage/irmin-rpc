open Capnp_rpc_lwt
open Irmin

(* Concrete types for an RPC store. *)
module Types = struct
  type t = Raw.Client.Store.t Capability.t

  type repo = Raw.Client.Repo.t Capability.t

  type commit = Raw.Client.Commit.t Capability.t
end

module type S = sig
  type t = Raw.Client.Irmin.t Capability.t
  (** A handle on an Irmin RPC server. *)

  module Store : sig
    (** Subset of the Irmin store API in which stores, repositories and commits
        are Capnp capabilities for optionally-remote values. *)

    include module type of Types
    (** @inline *)

    type tree

    type branch

    type key

    type contents

    type hash

    val master : repo -> t Lwt.t

    val of_branch : repo -> branch -> t Lwt.t

    val get : t -> key -> contents Lwt.t

    val find : t -> key -> (contents option, [> `Msg of string ]) result Lwt.t

    val find_tree : t -> key -> tree option Lwt.t

    val set : info:Irmin.Info.f -> t -> key -> contents -> unit Lwt.t

    val set_tree : info:Irmin.Info.f -> t -> key -> tree -> unit Lwt.t

    val remove : info:Info.f -> t -> key -> unit Lwt.t

    val merge_with_branch :
      t -> info:Info.f -> branch -> (unit, Merge.conflict) result Lwt.t

    module Branch : sig
      val list : repo -> branch list Lwt.t

      val remove : repo -> branch -> unit Lwt.t

      val set : repo -> branch -> commit -> unit Lwt.t
    end
  end

  val repo : t -> Store.repo Lwt.t

  val heartbeat :
    t -> string -> (string, [> `Capnp of Capnp_rpc.Error.t ]) result Lwt.t
end

module type MAKER = functor
  (Store : Irmin.S)
  (Endpoint_codec : Codec.SERIALISABLE with type t = Store.Private.Sync.endpoint)
  ->
  S
    with type Store.tree = Store.tree
     and type Store.branch = Store.branch
     and type Store.key = Store.key
     and type Store.contents = Store.contents
     and type Store.hash = Store.hash

module type Client = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
