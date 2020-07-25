(* 128-bit signed integer. *)

open Rudiments_int

type t
include Intnw_intf.S_i with type t := t

val to_tup: t -> u64 * u64
(** Convert to a little-endian tuple of words (first word is least significant).
*)
