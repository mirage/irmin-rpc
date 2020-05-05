open Raw

type tree_struct = Builder.Tree.struct_t

type info_struct = Builder.Info.struct_t

type commit_struct = Builder.Commit.Value.struct_t

module type SERIALISABLE = sig
  type t

  val encode : t -> string

  val decode : string -> (t, [> `Msg of string ]) result
end

module type MAKER = functor (Store : Irmin.S) -> sig
  module Branch : SERIALISABLE with type t = Store.branch

  module Key : SERIALISABLE with type t = Store.key

  module Hash : SERIALISABLE with type t = Store.hash

  module Contents : SERIALISABLE with type t = Store.contents

  module Info : sig
    type t = Irmin.Info.t

    val encode : Raw.Builder.Info.t -> t -> unit

    val decode : Raw.Reader.Info.t -> t
  end

  module Tree : sig
    type t = Store.tree

    val encode : tree_struct builder_t -> Store.key -> t -> unit Lwt.t

    val decode : tree_struct reader_t -> Store.Tree.concrete
  end

  module Commit : sig
    type t = Store.commit

    val encode : Raw.Builder.Commit.Value.t -> t -> unit Lwt.t

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

    val encode : Raw.Builder.Sync.PushResult.t -> t -> unit Lwt.t
  end

  val encode_commit_info : Store.commit -> info_struct builder_t -> unit
end

module type Codec = sig
  module type SERIALISABLE = SERIALISABLE

  module Unit : SERIALISABLE with type t = unit

  module type MAKER = MAKER

  module Make : MAKER
end
