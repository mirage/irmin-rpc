module Maker =
  Irmin_pack.Maker
    (struct
      let version = `V1
    end)
    (struct
      let stable_hash = 32

      let entries = 32
    end)

module Store = Maker.Make
    (Irmin.Metadata.None)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)

module Rpc =
  Irmin_rpc_unix.Make
    (Store)
    (Irmin_rpc.Config.Remote.None (Store))
    (Irmin_rpc.Config.Pack.Make (Store))
