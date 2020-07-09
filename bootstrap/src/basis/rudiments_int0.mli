type i64 = int64
type u64 = int64
type u128 = {
  hi: u64;
  lo: u64;
}
type isize
type usize = int

val usize_of_isize: isize -> usize
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val isize_of_usize: usize -> isize
(** Convert an unsigned integer to a bitwise identical unsigned integer. *)

val int_of_isize: isize -> int
(** Convert a signed integer to a bitwise identical OCaml integer. *)

val isize_of_int: int -> isize
(** Convert a OCaml integer to a bitwise identical signed integer. *)

val u128_of_usize: usize -> u128
(** Convert unsigned integer to 128-bit unsigned integer. *)

val u128_pp_x: Format.formatter -> u128 -> unit
(** [u128_pp_x ppf u] prints a hexadecimal representation of [u] to the pretty
    printing formatter, [ppf].  This function is intended for use with the [%a]
    format specifier to {!Format.printf}.*)

val u128_zero: u128
(** Zero constant. *)

val u128_one: u128
(** One constant. *)

val u128_bit_or: u128 -> u128 -> u128
(** Bitwise or. *)

val u128_bit_sl: shift:usize -> u128 -> u128
(** Bit shift left. *)

val u128_bit_usr: shift:usize -> u128 -> u128
(** Unsigned bit shift right (no sign extension). *)

val u128_add: u128 -> u128 -> u128
(** Addition. *)

val u128_mul: u128 -> u128 -> u128
(** Multiplication. *)

val u128_of_string: string -> u128
(** Convert string to u128, or halt on error. *)
