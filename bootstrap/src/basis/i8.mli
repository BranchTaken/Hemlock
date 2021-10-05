(** 8-bit signed integer.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

type t
include IntnbIntf.SI with type t := t

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

include ConvertIntf.Nb with type t := t

val trunc_of_i32: I32.t -> t
val extend_to_i32: t -> I32.t
val narrow_of_i32_opt: I32.t -> t option
val narrow_of_i32_hlt: I32.t -> t

val trunc_of_u32: U32.t -> t
val narrow_of_u32_opt: U32.t -> t option
val widen_to_u32_opt: t -> U32.t option
val narrow_of_u32_hlt: U32.t -> t
val widen_to_u32_hlt: t -> U32.t

val trunc_of_i16: I16.t -> t
val extend_to_i16: t -> I16.t
val narrow_of_i16_opt: I16.t -> t option
val narrow_of_i16_hlt: I16.t -> t

val trunc_of_u16: U16.t -> t
val narrow_of_u16_opt: U16.t -> t option
val widen_to_u16_opt: t -> U16.t option
val narrow_of_u16_hlt: U16.t -> t
val widen_to_u16_hlt: t -> U16.t
