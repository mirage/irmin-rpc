open Raw

type tree_struct = Builder.Irmin.Tree.struct_t

type info_struct = Builder.Irmin.Info.struct_t

type commit_struct = Builder.Irmin.Commit.struct_t

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

  val encode_tree :
    tree_struct builder_t -> Store.key -> Store.tree -> unit Lwt.t

  val decode_tree : tree_struct reader_t -> Store.Tree.concrete

  val encode_commit_info : Store.commit -> info_struct builder_t -> unit

  val encode_commit : commit_struct builder_t -> Store.commit -> unit Lwt.t
end

module type Codec = sig
  module type MAKER = MAKER

  module Make : MAKER
end
