(** Hash type. *)

open Rudiments_uint0

type t
(** Hash type. *)

type state
(** Hash state type. *)

val state_of_uint: uint -> state
(** Initialize hash state. *)

val t_of_state: state -> t
(** Return hash value associated with state. *)

val pp: Format.formatter -> t -> unit
(** [pp ppf t] prints a decimal representation of [t] to the pretty printing
    formatter, [ppf].  This function is intended for use with the [%a] format
    specifier to {!Format.printf}. *)

val pp_x: Format.formatter -> t -> unit
(** [pp_x ppf t] prints a hexadecimal representation of [t] to the pretty
    printing formatter, [ppf].  This function is intended for use with the [%a]
    format specifier to {!Format.printf}. *)

val hash_fold: state -> 'a -> state
(** [hash_fold state a] incorporates the hash of [a] into [state] and returns
    the resulting state. *)

val hash: 'a -> t
(** [hash a] returns the hash of [a]. *)
