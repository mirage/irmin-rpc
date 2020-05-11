val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Left-to-right function composition. *)

module Option : sig
  include module type of Option

  val iter_lwt : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

  val map_lwt : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t
end

module String : sig
  include module type of String

  val to_list : string -> char list
  (** Convert a string to a list of characters. *)

  val is_substring : string -> string -> bool
  (** [is_substring "bar" "foo bar baz"] is [true]. *)
end
