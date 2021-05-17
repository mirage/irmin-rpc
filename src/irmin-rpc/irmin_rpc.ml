module Codec = Codec
module Config = Config
module Client = Client
module Server = Server

module Private = struct
  module Utils = Utils
end

module Make
    (Store : Irmin.S)
    (Remote : Config.REMOTE with type t = Store.Private.Remote.endpoint)
    (Pack : Config.PACK with type repo = Store.repo) =
struct
  module Client = Client.Make (Store) (Remote) (Pack)
  module Server = Server.Make (Store) (Remote) (Pack)
end
