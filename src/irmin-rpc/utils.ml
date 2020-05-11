open Lwt.Infix

let ( >> ) f g x = g (f x)

module Option = struct
  include Option

  let iter_lwt f = function Some v -> f v | None -> Lwt.return_unit

  let map_lwt f = function Some v -> f v >|= some | None -> Lwt.return_none
end
