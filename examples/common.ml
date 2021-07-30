module Maker =
  Irmin_pack.Maker
    (struct
      let version = `V1
    end)
    (struct
      let stable_hash = 32

      let entries = 32
    end)

module Store = Maker.Make (struct
  module Info = Irmin.Info.Default
  module Metadata = Irmin.Metadata.None
  module Contents = Irmin.Contents.String
  module Path = Irmin.Path.String_list
  module Branch = Irmin.Branch.String
  module Hash = Irmin.Hash.BLAKE2B
  module Node = Irmin.Node.Make (Hash) (Path) (Metadata)
  module Commit = Irmin.Commit.Make (Hash)
end)

module Rpc =
  Irmin_rpc_unix.Make
    (Store)
    (Irmin_rpc.Config.Remote.None (Store))
    (Irmin_rpc.Config.Pack.Make (Store))
