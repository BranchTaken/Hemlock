(** 63-bit unsigned integer type. *)

open Rudiments_uint

type t = uint
include Intnb_intf.S_u with type t := t

val to_int: t -> int
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_int: int -> t
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val kv: int -> t
(** Create a constant value. *)
