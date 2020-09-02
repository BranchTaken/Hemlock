(* 256-bit signed integer. *)

open RudimentsInt

type t
include IntwIntf.SFI with type t := t

val to_tup: t -> u64 * u64 * u64 * u64
(** Convert to a little-endian tuple of words (first word is least significant).
*)
