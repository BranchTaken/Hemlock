(** 64-bit signed integer type.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

open RudimentsInt0

type t = sint

include IntnbIntf.S with type t := t
include IntnbIntf.SSigned with type t := t

val trunc_of_i512: I512.t -> t
val extend_to_i512: t -> I512.t
val narrow_of_i512_opt: I512.t -> t option
val narrow_of_i512_hlt: I512.t -> t

val trunc_of_u512: U512.t -> t
val narrow_of_u512_opt: U512.t -> t option
val widen_to_u512_opt: t -> U512.t option
val narrow_of_u512_hlt: U512.t -> t
val widen_to_u512_hlt: t -> U512.t

val trunc_of_i256: I256.t -> t
val extend_to_i256: t -> I256.t
val narrow_of_i256_opt: I256.t -> t option
val narrow_of_i256_hlt: I256.t -> t

val trunc_of_u256: U256.t -> t
val narrow_of_u256_opt: U256.t -> t option
val widen_to_u256_opt: t -> U256.t option
val narrow_of_u256_hlt: U256.t -> t
val widen_to_u256_hlt: t -> U256.t

val trunc_of_i128: I128.t -> t
val extend_to_i128: t -> I128.t
val narrow_of_i128_opt: I128.t -> t option
val narrow_of_i128_hlt: I128.t -> t

val trunc_of_u128: U128.t -> t
val narrow_of_u128_opt: U128.t -> t option
val widen_to_u128_opt: t -> U128.t option
val narrow_of_u128_hlt: U128.t -> t
val widen_to_u128_hlt: t -> U128.t

(**************************************************************************************************)

val kv: int64 -> t
(** Create a constant value. *)

val extend_of_int: int -> t
(** Initialize from a default-width integer. *)

val trunc_to_int: t -> int
(** Convert to a default-width integer, with possible loss. *)

val narrow_to_int_opt: t -> int option
(** Convert to a default-width integer, or return [None] if conversion would be lossy. *)

val narrow_to_int_hlt: t -> int
(** Convert to a default-width integer, or halt if conversion would be lossy. *)
