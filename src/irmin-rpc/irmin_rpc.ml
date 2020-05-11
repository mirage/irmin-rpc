module Client = Client
module Codec = Codec
module Server = Server

module Private = struct
  module Utils = Utils
end

module Make
    (Store : Irmin.S)
    (Endpoint_codec : Codec.SERIALISABLE
                        with type t = Store.Private.Sync.endpoint) =
struct
  module Client = Client.Make (Store) (Endpoint_codec)
  module Server = Server.Make (Store) (Endpoint_codec)
end
