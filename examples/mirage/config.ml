open Mirage

let main =
  foreign
  ~packages:[
    package "duration";
    package "irmin-rpc-mirage";
    package "digestif.ocaml";
    package "charrua-client-mirage";
  ]
  ~deps:[
    abstract nocrypto;
  ]
  "Unikernel.Main" (pclock @-> time @-> stackv4 @-> job)


let stack = static_ipv4_stack default_network

let () = register "irmin-rpc" [main $ default_posix_clock $ default_time $ stack]
