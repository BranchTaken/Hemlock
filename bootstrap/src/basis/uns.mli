(** 64-bit unsigned integer type.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

open RudimentsInt0

type t = uns

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

(**************************************************************************************************)

val bits_of_i64: sint -> t
(** [bits_of_i64 x] converts [x] to a bitwise identical {!type:u64} value. *)

val bits_to_i64: t -> sint
(** [to_i64_bits t] converts [t] to a bitwise identical {!type:i64} value. *)

val like_of_i64_opt: sint -> t option
(** [like_of_i64_opt x] converts [x] to an equivalent {!type:u64} value, or [None] if [x] is outside
    the {!type:u64} range. *)

val like_to_i64_opt: t -> sint option
(** [like_to_i64_opt t] converts [t] to an equivalent {!type:i64} value, or [None] if [t] is outside
    the {!type:i64} range. *)

val like_of_i64_hlt: sint -> t
(** [like_of_i64_hlt x] converts [x] to an equivalent {!type:u64} value, or halts if [x] is outside
    the {!type:u64} range. *)

val like_to_i64_hlt: t -> sint
(** [like_to_i64_hlt t] converts [t] to an equivalent {!type:i64} value, or halts if [t] is outside
    the {!type:i64} range. *)

(**************************************************************************************************)

val bits_of_sint: sint -> t
(** [bits_of_sint x] converts [x] to a bitwise identical {!type:uns} value. *)

val bits_to_sint: t -> sint
(** [bits_to_sint t] converts [t] to a bitwise identical {!type:sint} value. *)

val like_of_sint_opt: sint -> t option
(** [like_of_sint_opt x] converts [x] to an equivalent {!type:u64} value, or [None] if [x] is
    outside the {!type:u64} range. *)

val like_to_sint_opt: t -> sint option
(** [like_to_sint_opt t] converts [t] to an equivalent {!type:sint} value, or [None] if [t] is
    outside the {!type:sint} range. *)

val like_of_sint_hlt: sint -> t
(** [like_of_sint_hlt x] converts [x] to an equivalent {!type:u64} value, or halts if [x] is outside
    the {!type:u64} range. *)

val like_to_sint_hlt: t -> sint
(** [like_to_sint_hlt t] converts [t] to an equivalent {!type:sint} value, or halts if [t] is
    outside the {!type:sint} range. *)

(**************************************************************************************************)

val extend_of_int: int -> t
(** Initialize from a default-width integer. *)

val trunc_to_int: t -> int
(** Convert to default-width integer, with possible loss. *)

val narrow_to_int_opt: t -> int option
(** Convert to default-width integer, or return [None] if conversion would be lossy. *)

val narrow_to_int_hlt: t -> int
(** Convert to default-width integer, or halt if conversion would be lossy. *)
