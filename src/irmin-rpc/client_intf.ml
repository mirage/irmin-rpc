open Capnp_rpc_lwt
open Irmin

(* Concrete types for an RPC store. *)
module Types = struct
  type store = Raw.Client.Store.t Capability.t

  type repo = Raw.Client.Repo.t Capability.t

  type commit = Raw.Client.Commit.t Capability.t

  type sync = Raw.Client.Sync.t Capability.t

  type pack = Raw.Client.Pack.t Capability.t

  type tree = Raw.Client.Tree.t Capability.t

  type tx = Raw.Client.Tx.t Capability.t
end

module type S = sig
  type t = Raw.Client.Irmin.t Capability.t
  (** A handle on an Irmin RPC server. *)

  include module type of Types
  (** @inline *)

  type branch
  (** Branch type, inherited from the underlying Irmin store *)

  type key
  (** Key type, inherited from the underlying Irmin store *)

  type contents
  (** Value type, inherited from the underlying Irmin store *)

  type hash
  (** Hash type, inherited from the underlying Irmin store *)

  type step
  (** Key step, inherited from the underlying Irmin store *)

  module Key : Irmin.Path.S with type t = key and type step = step

  module Hash : Irmin.Hash.S with type t = hash

  module Store : sig
    type t = store
    (** Subset of the Irmin store API in which stores, repositories and commits
        are Capnp capabilities for optionally-remote values. *)

    val master : repo -> t Lwt.t
    (** Access the [master] branch *)

    val of_branch : repo -> branch -> t Lwt.t
    (** Access a specific branch *)

    val get : t -> key -> contents Lwt.t
    (** Get value of specified key *)

    val find : t -> key -> (contents option, [> `Msg of string ]) result Lwt.t
    (** Get value of specified key, returning an option result to avoid raising
        exceptions *)

    val find_hash : t -> key -> hash option Lwt.t
    (** Get the hash of the contents stored at [key], if the key exists *)

    val find_tree : t -> key -> tree option Lwt.t
    (** Find a tree from the Irmin store, if it exists *)

    val get_tree : t -> key -> tree Lwt.t
    (** Get a tree from the Irmin store *)

    val set : info:Irmin.Info.f -> t -> key -> contents -> unit Lwt.t
    (** Set a value at the specified key *)

    val test_and_set :
      info:Irmin.Info.f ->
      t ->
      key ->
      test:contents option ->
      set:contents option ->
      bool Lwt.t

    val set_tree : info:Irmin.Info.f -> t -> key -> tree -> unit Lwt.t
    (** Set a tree at the specified key *)

    val test_and_set_tree :
      info:Irmin.Info.f ->
      t ->
      key ->
      test:tree option ->
      set:tree option ->
      bool Lwt.t

    val remove : info:Info.f -> t -> key -> unit Lwt.t
    (** Remove the specified key *)

    val merge_with_branch :
      t -> info:Info.f -> branch -> (unit, Merge.conflict) result Lwt.t
    (** Merge the current branch with another branch *)

    val sync : t -> sync option Lwt.t
    (** Get [sync] capability *)

    val pack : t -> pack option Lwt.t
    (** Get [pack] capability *)

    val last_modified : t -> key -> commit option Lwt.t
    (** Returns the last commit containing changes to the specified key *)
  end

  module Branch : sig
    include Irmin.Branch.S with type t = branch

    val list : repo -> branch list Lwt.t
    (** List all branches *)

    val remove : repo -> branch -> unit Lwt.t
    (** Remove a branch *)

    val set : repo -> branch -> commit -> unit Lwt.t
    (** Set branch to points to the provided commit *)
  end

  module Commit : sig
    type t = commit

    val v : repo -> info:Irmin.Info.f -> parents:hash list -> tree -> t Lwt.t

    val check : commit -> bool Lwt.t

    val of_hash : repo -> hash -> commit option Lwt.t
    (** Get commit from specified hash *)

    val hash : commit -> hash Lwt.t
    (** Get commit hash *)

    val info : commit -> Irmin.Info.t Lwt.t
    (** Get commit info *)

    val parents : commit -> hash list Lwt.t
    (** Get commit parent hashes *)

    val tree : commit -> tree Lwt.t
    (** Get commit tree *)
  end

  module Sync : sig
    type endpoint
    (** Sync endpoint type *)

    type t = sync

    val clone : sync -> endpoint -> commit Lwt.t
    (** Clone remote repository *)

    val pull : sync -> info:Irmin.Info.f -> endpoint -> commit Lwt.t
    (** Pull from remote repository *)

    val push :
      sync ->
      endpoint ->
      ([ `Empty | `Head of hash ], [ `Detached_head | `Msg of string ]) result
      Lwt.t
    (** Push to remote repository *)
  end

  module Pack : sig
    type t = pack

    val integrity_check :
      ?auto_repair:bool ->
      pack ->
      ( [ `No_error | `Fixed of int ],
        [ `Corrupted of int | `Cannot_fix of string ] )
      result
      Lwt.t
    (** Perform pack/index integrity check *)
  end

  module Tree : sig
    type t = tree

    type concrete = [ `Contents of hash | `Tree of (step * concrete) list ]

    val empty : repo -> tree Lwt.t
    (** Create an empty tree *)

    val check : tree -> bool Lwt.t

    val find : tree -> key -> contents option Lwt.t
    (** Find value associated with key *)

    val find_tree : tree -> key -> tree option Lwt.t
    (** Get a subtree *)

    val get_tree : t -> key -> tree Lwt.t
    (** Get a subtree *)

    val mem : tree -> key -> bool Lwt.t
    (** Check if contents exist at the given key *)

    val mem_tree : tree -> key -> bool Lwt.t
    (** Check if a subtree exists at the given key *)

    val concrete : tree -> concrete Lwt.t
    (** Return a concrete representation of a tree *)

    val of_concrete : repo -> concrete -> tree Lwt.t
    (** Create a remote tree from a [concrete] tree *)

    val find_hash : tree -> key -> hash option Lwt.t
    (** Get hash of contents at the given key *)

    val list : tree -> key -> step list Lwt.t
  end

  module Tx : sig
    type t = tx

    val v : repo -> tree -> tx Lwt.t
    (** Convert a [Tree] into an editable [Tx] *)

    val remove : tx -> key -> unit Lwt.t
    (** Remove a key from [Tx] *)

    val add : tx -> key -> contents -> unit Lwt.t
    (** Add new key/value to [Tx] *)

    val add_tree : tx -> key -> tree -> unit Lwt.t
    (** Add subtree *)

    val tree : tx -> tree Lwt.t
    (** Get [Tree] from [Tx] *)
  end

  module Contents : sig
    include Irmin.Contents.S with type t = contents

    val import : repo -> contents list -> hash list Lwt.t
    (** import contents to the repo, returning a hash for each item *)

    val of_hash : repo -> hash -> contents option Lwt.t
    (** Get hash from contents, this function uses caching to avoid calling to
        the server for every request *)

    val find : store -> key -> (repo -> contents Lwt.t) option Lwt.t
    (** Returns a function that returns the contents associated with the
        specified key, if available *)
  end

  val repo : t -> repo Lwt.t
  (** Acquire a repo handle *)

  val ping : t -> (unit, [> `Capnp of Capnp_rpc.Error.t ]) result Lwt.t
  (** Ping server *)
end

module type MAKER = functor
  (Store : Irmin.S)
  (Remote : Config.REMOTE with type t = Store.Private.Sync.endpoint)
  (Pack : Config.PACK with type repo = Store.repo)
  ->
  S
    with type branch = Store.branch
     and type key = Store.key
     and type contents = Store.contents
     and type hash = Store.hash
     and type Sync.endpoint = Remote.t
     and type step = Store.Key.step
     and module Key = Store.Key
     and module Hash = Store.Hash

module type Client = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
