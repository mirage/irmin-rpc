open Raw

type info_struct = Builder.Info.struct_t

type commit_struct = Builder.Commit.Value.struct_t

type ('hash, 'step) concrete_tree =
  [ `Contents of 'hash | `Tree of ('step * ('hash, 'step) concrete_tree) list ]

module type SERIALISABLE = sig
  type t

  val encode : t -> string

  val decode : string -> (t, [> `Msg of string ]) result
end

module type MAKER = functor (Store : Irmin.S) -> sig
  module Branch : SERIALISABLE with type t = Store.branch

  module Key : sig
    module Step : SERIALISABLE with type t = Store.step

    include SERIALISABLE with type t = Store.key
  end

  module Hash : SERIALISABLE with type t = Store.hash

  module Contents : SERIALISABLE with type t = Store.contents

  module Info : sig
    type t = Store.Info.t

    val encode : t -> Raw.Builder.Info.t

    val decode : Raw.Reader.Info.t -> t
  end

  module Tree : sig
    type t = (Store.hash, Store.step) concrete_tree

    val of_irmin_tree : Store.tree -> t Lwt.t

    val to_irmin_tree : Store.repo -> t -> Store.tree Lwt.t

    val encode : t -> Raw.Builder.Tree.Concrete.t Lwt.t

    val decode : Raw.Reader.Tree.Concrete.t -> t
  end

  module Commit : sig
    type t = Store.commit

    val encode : t -> Raw.Builder.Commit.Value.t Lwt.t

    val decode :
      Store.repo ->
      Raw.Reader.Commit.Value.t ->
      (t, [> `Msg of string | `Commit_not_found of Store.hash ]) result Lwt.t
  end

  module Merge_result : sig
    type t = (unit, Irmin.Merge.conflict) result

    val encode : Raw.Builder.Store.MergeResult.t -> t -> unit

    val decode :
      Raw.Reader.Store.MergeResult.t -> (t, [ `Msg of string ]) result
  end

  module Push_result : sig
    type t =
      ( [ `Empty | `Head of Store.commit ],
        [ `Detached_head | `Msg of string ] )
      result

    val encode : t -> Raw.Builder.Remote.PushResult.t Lwt.t
  end

  val encode_commit_info : Store.commit -> info_struct builder_t -> unit
end

module type Codec = sig
  module type SERIALISABLE = SERIALISABLE

  module Unit : SERIALISABLE with type t = unit

  module type MAKER = MAKER

  module Make : MAKER
end
