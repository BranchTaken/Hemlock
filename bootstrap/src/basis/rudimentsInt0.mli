type i64 = int64
type u64 = int64
type u128
type sint
type uns = int

val uns_of_sint: sint -> uns
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val sint_of_uns: uns -> sint
(** Convert an unsigned integer to a bitwise identical unsigned integer. *)

val int_of_sint: sint -> int
(** Convert a signed integer to a bitwise identical OCaml integer. *)

val sint_of_int: int -> sint
(** Convert an OCaml integer to a bitwise identical signed integer. *)

val u128_init: f:(uns -> u64) -> u128
(** [init ~f] initializes a {!type:u128}, where [f] provides the value for each
    element at given little-endian index. *)

val u128_get: uns -> u128 -> u64
(** Get word at given little-endian index. *)

val u128_of_tup: u64 * u64-> u128
(** Initialize from a little-endian tuple of words (first word is least
    significant). *)

val u128_to_tup: u128 -> u64 * u64
(** Convert to a little-endian tuple of words (first word is least significant).
*)

val u128_of_uns: uns -> u128
(** Convert unsigned integer to 128-bit unsigned integer. *)

val u128_pp_x: Format.formatter -> u128 -> unit
(** [u128_pp_x ppf u] prints a hexadecimal representation of [u] to the pretty
    printing formatter, [ppf]. This function is intended for use with the [%a]
    format specifier to {!Format.printf}.*)

val u128_compare: u128 -> u128 -> sint
(** [u128_compare a b] returns [{-1, 0, 1}] if [a {<,=,>} b], respectively. *)

val u128_zero: u128
(** Zero constant. *)

val u128_one: u128
(** One constant. *)

val u128_bit_or: u128 -> u128 -> u128
(** Bitwise or. *)

val u128_bit_sl: shift:uns -> u128 -> u128
(** Bit shift left. *)

val u128_bit_usr: shift:uns -> u128 -> u128
(** Unsigned bit shift right (no sign extension). *)

val u128_add: u128 -> u128 -> u128
(** Addition. *)

val u128_mul: u128 -> u128 -> u128
(** Multiplication. *)

val u128_of_string: string -> u128
(** Convert string to u128, or halt on error. *)
