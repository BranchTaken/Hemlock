type uint

val uint_of_int: int -> uint
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val int_of_uint: uint -> int
(** Convert an unsigned integer to a bitwise identical unsigned integer. *)

val kv: int -> uint
(** Create a constant value. *)
