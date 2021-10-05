(** 128-bit unsigned integer type.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

open RudimentsInt0

type t = u128
include IntwIntf.SFU with type t := t

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

val bits_of_i128: I128.t -> t
val bits_to_i128: t -> I128.t
val like_of_i128_opt: I128.t -> t option
val like_to_i128_opt: t -> I128.t option
val like_of_i128_hlt: I128.t -> t
val like_to_i128_hlt: t -> I128.t
