(** 63-bit unsigned integer type. *)

open Rudiments_int0

type t = uns
include Intnb_intf.S_u with type t := t

val to_isize: t -> isize
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_isize: isize -> t
(** Convert a signed integer to a bitwise identical unsigned integer. *)
