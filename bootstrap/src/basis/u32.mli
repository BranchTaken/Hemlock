(** 32-bit unsigned integer.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

type t
include IntnbIntf.SU with type t := t

val trunc_of_i512: I512.t -> t
val extend_to_i512: t -> I512.t
val narrow_of_i512_opt: I512.t -> t option
val narrow_of_i512_hlt: I512.t -> t

val trunc_of_u512: U512.t -> t
val extend_to_u512: t -> U512.t
val narrow_of_u512_opt: U512.t -> t option
val narrow_of_u512_hlt: U512.t -> t

val trunc_of_i256: I256.t -> t
val extend_to_i256: t -> I256.t
val narrow_of_i256_opt: I256.t -> t option
val narrow_of_i256_hlt: I256.t -> t

val trunc_of_u256: U256.t -> t
val extend_to_u256: t -> U256.t
val narrow_of_u256_opt: U256.t -> t option
val narrow_of_u256_hlt: U256.t -> t

val trunc_of_i128: I128.t -> t
val extend_to_i128: t -> I128.t
val narrow_of_i128_opt: I128.t -> t option
val narrow_of_i128_hlt: I128.t -> t

val trunc_of_u128: U128.t -> t
val extend_to_u128: t -> U128.t
val narrow_of_u128_opt: U128.t -> t option
val narrow_of_u128_hlt: U128.t -> t

include ConvertIntf.Nb with type t := t

val bits_of_i32: I32.t -> t
val bits_to_i32: t -> I32.t
val like_of_i32_opt: I32.t -> t option
val like_to_i32_opt: t -> I32.t option
val like_of_i32_hlt: I32.t -> t
val like_to_i32_hlt: t -> I32.t
