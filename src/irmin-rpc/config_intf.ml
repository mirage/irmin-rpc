module type REMOTE = sig
  type t

  val v : (module Codec.SERIALISABLE with type t = t) option
end

module type PACK = sig
  type repo

  val v : (module Irmin_pack.Store.S with type repo = repo) option
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
    module Make : functor (P : Irmin_pack.Store.S) ->
      PACK with type repo = P.repo

    module None (Store : Irmin.S) : PACK with type repo = Store.repo
  end
end
