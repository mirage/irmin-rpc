open Mirage

let main =
  foreign
    ~packages:[ package "duration"; package "irmin-rpc-mirage" ]
    "Unikernel.Main"
    (random @-> mclock @-> pclock @-> time @-> stackv4v6 @-> job)

let stack = static_ipv4v6_stack default_network

let packages = [ package "digestif" ]

let () =
  register ~packages "irmin-rpc"
    [
      main
      $ default_random
      $ default_monotonic_clock
      $ default_posix_clock
      $ default_time
      $ stack;
    ]
