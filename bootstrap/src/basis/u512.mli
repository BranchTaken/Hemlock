(* 512-bit unsigned integer. *)

open Rudiments_int

type t
include Intnw_intf.S_u with type t := t

val to_tup: t -> u64 * u64 * u64 * u64 * u64 * u64 * u64 * u64
(** Convert to a little-endian tuple of words (first word is least significant).
*)
