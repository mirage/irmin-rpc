open Capnp_rpc_lwt
open Irmin

(* Concrete types for an RPC store. *)
module Types = struct
  type t = Raw.Client.Store.t Capability.t

  type repo = Raw.Client.Repo.t Capability.t

  type commit = Raw.Client.Commit.t Capability.t

  type sync = Raw.Client.Sync.t Capability.t

  type pack = Raw.Client.Pack.t Capability.t
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

    type sync

    type pack

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

    val sync : t -> sync option Lwt.t

    val pack : t -> pack option Lwt.t

    val last_modified : t -> key -> commit Lwt.t

    module Branch : sig
      val list : repo -> branch list Lwt.t

      val remove : repo -> branch -> unit Lwt.t

      val set : repo -> branch -> commit -> unit Lwt.t
    end

    module Commit : sig
      val of_hash : repo -> hash -> commit Lwt.t

      val hash : commit -> hash Lwt.t

      val info : commit -> Irmin.Info.t Lwt.t

      val parents : commit -> hash list Lwt.t

      val tree : commit -> tree Lwt.t
    end

    module Sync : sig
      type endpoint

      val clone : sync -> endpoint -> commit Lwt.t

      val pull : sync -> info:Irmin.Info.f -> endpoint -> commit Lwt.t

      val push :
        sync ->
        endpoint ->
        ([ `Empty | `Head of hash ], [ `Detached_head | `Msg of string ]) result
        Lwt.t
    end

    module Pack : sig
      val integrity_check :
        ?auto_repair:bool ->
        pack ->
        ( [ `No_error | `Fixed of int ],
          [ `Corrupted of int | `Cannot_fix of string ] )
        result
        Lwt.t
    end
  end

  val repo : t -> Store.repo Lwt.t

  val ping : t -> (unit, [> `Capnp of Capnp_rpc.Error.t ]) result Lwt.t
end

module type MAKER = functor
  (Store : Irmin.S)
  (Remote : Config.REMOTE with type t = Store.Private.Sync.endpoint)
  (Pack : Config.PACK with type repo = Store.repo)
  ->
  S
    with type Store.tree = Store.tree
     and type Store.branch = Store.branch
     and type Store.key = Store.key
     and type Store.contents = Store.contents
     and type Store.hash = Store.hash
     and type Store.Sync.endpoint = Remote.t

module type Client = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
