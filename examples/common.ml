module Store =
  Irmin_pack.Make
    (struct
      let stable_hash = 256

      let entries = 32
    end)
    (Irmin.Metadata.None)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)

module Rpc =
  Irmin_rpc_unix.Make
    (Store)
    (Irmin_rpc.Config.Remote.None (Store))
    (Irmin_rpc.Config.Pack.Make (Store))
