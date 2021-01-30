open Lwt.Infix

let ( >> ) f g x = g (f x)

module Option = struct
  let iter_lwt f = function Some v -> f v | None -> Lwt.return_unit

  let map_lwt f = function
    | Some v -> f v >|= Option.some
    | None -> Lwt.return_none

  include Option
end

module String = struct
  let to_list s =
    let rec loop acc i = if i < 0 then acc else loop (s.[i] :: acc) (i - 1) in
    loop [] (String.length s - 1)

  let is_substring sub s =
    let rec inner ~from chars =
      match chars with
      | [] -> true
      | c :: cs -> (
          from < String.length s
          &&
          match String.index_from_opt s from c with
          | Some i -> inner ~from:(i + 1) cs
          | None -> false )
    in
    inner ~from:0 (to_list sub)

  include String
end
