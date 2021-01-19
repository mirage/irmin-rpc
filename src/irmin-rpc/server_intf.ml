type 'a cap = 'a Capnp_rpc_lwt.Capability.t

module type S = sig
  type repo

  type store

  type commit

  type hash

  module Commit : sig
    type t = Raw.Client.Commit.t cap

    val local : commit -> t

    val read :
      repo ->
      t ->
      (commit, [> `Msg of string | `Commit_not_found of hash ]) result Lwt.t
  end

  module Store : sig
    type t = Raw.Client.Store.t cap

    val local : store -> t
  end

  module Repo : sig
    type t = Raw.Client.Repo.t cap

    val local : repo -> t
  end

  val local : repo -> Raw.Client.Irmin.t cap
end

module type MAKER = functor
  (Store : Irmin.S)
  (Remote : Config_intf.REMOTE with type t = Store.Private.Sync.endpoint)
  (Pack : Config_intf.PACK with type repo = Store.repo)
  ->
  S
    with type repo = Store.repo
     and type store = Store.t
     and type commit = Store.commit
     and type hash = Store.hash

module type Server = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
