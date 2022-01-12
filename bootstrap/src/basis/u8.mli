(** 8-bit unsigned integer.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

type t
include IntnbIntf.SU with type t := t

val trunc_of_zint: Zint.t -> t
val extend_to_zint: t -> Zint.t
val narrow_of_zint_opt: Zint.t -> t option
val narrow_of_zint_hlt: Zint.t -> t

val trunc_of_nat: Nat.t -> t
val extend_to_nat: t -> Nat.t
val narrow_of_nat_opt: Nat.t -> t option
val narrow_of_nat_hlt: Nat.t -> t

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

val of_char: char -> t
(** Initialize from character literal. This is a stopgap for the lack of codepoint literals. *)

val trunc_of_i32: I32.t -> t
val extend_to_i32: t -> I32.t
val narrow_of_i32_opt: I32.t -> t option
val narrow_of_i32_hlt: I32.t -> t

val trunc_of_u32: U32.t -> t
val extend_to_u32: t -> U32.t
val narrow_of_u32_opt: U32.t -> t option
val narrow_of_u32_hlt: U32.t -> t

val trunc_of_i16: I16.t -> t
val extend_to_i16: t -> I16.t
val narrow_of_i16_opt: I16.t -> t option
val narrow_of_i16_hlt: I16.t -> t

val trunc_of_u16: U16.t -> t
val extend_to_u16: t -> U16.t
val narrow_of_u16_opt: U16.t -> t option
val narrow_of_u16_hlt: U16.t -> t

val bits_of_i8: I8.t -> t
val bits_to_i8: t -> I8.t
val like_of_i8_opt: I8.t -> t option
val like_to_i8_opt: t -> I8.t option
val like_of_i8_hlt: I8.t -> t
val like_to_i8_hlt: t -> I8.t
