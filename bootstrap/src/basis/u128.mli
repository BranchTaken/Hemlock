(** 128-bit unsigned integer type. *)

open RudimentsInt0

type t = u128
include IntwIntf.SFU with type t := t

val to_tup: t -> u64 * u64
(** Convert to a little-endian tuple of words (first word is least significant). *)
