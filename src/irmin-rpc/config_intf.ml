module type REMOTE = sig
  type t

  val v : (module Codec.SERIALISABLE with type t = t) option
end

module type STORE = sig
  type repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
end

module type PACK = sig
  type repo

  val v : (module STORE with type repo = repo) option
end

module type Config = sig
  module type REMOTE = REMOTE

  module type PACK = PACK

  module Remote : sig
    module Make : functor (C : Codec.SERIALISABLE) -> REMOTE with type t = C.t

    module None (Store : Irmin.S) :
      REMOTE with type t = Store.Private.Sync.endpoint
  end

  module Pack : sig
    module type STORE = STORE

    module Make : functor (S : STORE) -> PACK with type repo = S.repo

    module None (Store : Irmin.S) : PACK with type repo = Store.repo
  end
end
