type isize
type usize = int (* XXX Temporarily make opaque to find hidden errors. *)

val usize_of_isize: isize -> usize
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val isize_of_usize: usize -> isize
(** Convert an unsigned integer to a bitwise identical unsigned integer. *)

val int_of_isize: isize -> int
(** Convert a signed integer to a bitwise identical OCaml integer. *)

val isize_of_int: int -> isize
(** Convert a OCaml integer to a bitwise identical signed integer. *)
