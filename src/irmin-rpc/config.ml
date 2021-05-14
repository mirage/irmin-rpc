include Config_intf

module Remote = struct
  module Make (C : Codec.SERIALISABLE) = struct
    type t = C.t

    let v = Some (module C : Codec.SERIALISABLE with type t = C.t)
  end

  module None (Store : Irmin.S) = struct
    type t = Store.Private.Remote.endpoint

    let v = None
  end
end

module Pack = struct
  module type STORE = STORE

  module Make (P : STORE) = struct
    type repo = P.repo

    let v = Some (module P : STORE with type repo = P.repo)
  end

  module None (Store : Irmin.S) = struct
    type repo = Store.repo

    let v = None
  end
end
