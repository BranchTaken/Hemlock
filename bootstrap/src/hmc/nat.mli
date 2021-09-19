(** Arbitrary-precision unsigned integer (ℕ₀). *)

open Basis
open Basis.Rudiments

type t
include IntwIntf.SVU with type t := t

val k_0: t
(** Return constant [0]. *)

val k_1: t
(** Return constant [1]. *)

val k_2: t
(** Return constant [2]. *)

val k_3: t
(** Return constant [3]. *)

val k_4: t
(** Return constant [4]. *)

val k_5: t
(** Return constant [5]. *)

val k_6: t
(** Return constant [6]. *)

val k_7: t
(** Return constant [7]. *)

val k_8: t
(** Return constant [8]. *)

val k_9: t
(** Return constant [9]. *)

val k_a: t
(** Return constant [10] ([0xa]). *)

val k_b: t
(** Return constant [11] ([0xb]). *)

val k_c: t
(** Return constant [12] ([0xc]). *)

val k_d: t
(** Return constant [13] ([0xd]). *)

val k_e: t
(** Return constant [14] ([0xe]). *)

val k_f: t
(** Return constant [15] ([0xf]). *)

val k_g: t
(** Return constant [16] ([0x10]). *)

val max_u8: t
(** Return the maximum value which can be converted to {!type:u8} without loss of precision. *)

val max_abs_i8: t
(** Return the maximum value which can be converted to the absolute value of the minimum {!type:i8}
    value without loss of precision. *)

val max_u16: t
(** Return the maximum value which can be converted to {!type:u16} without loss of precision. *)

val max_abs_i16: t
(** Return the maximum value which can be converted to the absolute value of the minimum {!type:i16}
    value without loss of precision. *)

val max_u32: t
(** Return the maximum value which can be converted to {!type:u32} without loss of precision. *)

val max_abs_i32: t
(** Return the maximum value which can be converted to the absolute value of the minimum {!type:i32}
    value without loss of precision. *)

val max_uns: t
(** Return the maximum value which can be converted to {!type:uns} without loss of precision. *)

val max_abs_int: t
(** Return the maximum value which can be converted to the absolute value of the minimum {!type:int}
    value without loss of precision. *)

val max_u64: t
(** Return the maximum value which can be converted to {!type:u64} without loss of precision. *)

val max_abs_i64: t
(** Return the maximum value which can be converted to the absolute value of the minimum {!type:i64}
    value without loss of precision. *)

val max_u128: t
(** Return the maximum value which can be converted to {!type:u128} without loss of precision. *)

val max_abs_i128: t
(** Return the maximum value which can be converted to the absolute value of the minimum
    {!type:i128} value without loss of precision. *)

val max_u256: t
(** Return the maximum value which can be converted to {!type:u256} without loss of precision. *)

val max_abs_i256: t
(** Return the maximum value which can be converted to the absolute value of the minimum
    {!type:i256} value without loss of precision. *)

val max_u512: t
(** Return the maximum value which can be converted to {!type:u512} without loss of precision. *)

val max_abs_i512: t
(** Return the maximum value which can be converted to the absolute value of the minimum
    {!type:i512} value without loss of precision. *)

val to_u8_opt: t -> u8 option
(** Convert to an unsigned 8-bit integer, or return [None] if conversion would be lossy. *)

val to_i8_opt: t -> i8 option
(** Convert to a signed 8-bit integer, or return [None] if conversion would be lossy. *)

val to_u16_opt: t -> u16 option
(** Convert to an unsigned 16-bit integer, or return [None] if conversion would be lossy. *)

val to_i16_opt: t -> i16 option
(** Convert to a signed 16-bit integer, or return [None] if conversion would be lossy. *)

val to_u32_opt: t -> u32 option
(** Convert to an unsigned 32-bit integer, or return [None] if conversion would be lossy. *)

val to_i32_opt: t -> i32 option
(** Convert to a signed 32-bit integer, or return [None] if conversion would be lossy. *)

val to_uns_opt: t -> uns option
(** Convert to an unsigned integer, or return [None] if conversion would be lossy. *)

val to_int_opt: t -> sint option
(** Convert to a signed integer, or return [None] if conversion would be lossy. *)

val to_u64_opt: t -> u64 option
(** Convert to an unsigned 64-bit integer, or return [None] if conversion would be lossy. *)

val to_i64_opt: t -> i64 option
(** Convert to a signed 8-bit integer, or return [None] if conversion would be lossy. *)

val to_u128_opt: t -> u128 option
(** Convert to an unsigned 128-bit integer, or return [None] if conversion would be lossy. *)

val to_i128_opt: t -> i128 option
(** Convert to a signed 128-bit integer, or return [None] if conversion would be lossy. *)

val to_u256_opt: t -> u256 option
(** Convert to an unsigned 256-bit integer, or return [None] if conversion would be lossy. *)

val to_i256_opt: t -> i256 option
(** Convert to a signed 256-bit integer, or return [None] if conversion would be lossy. *)

val to_u512_opt: t -> u512 option
(** Convert to an unsigned 512-bit integer, or return [None] if conversion would be lossy. *)

val to_i512_opt: t -> i512 option
(** Convert to a signed 512-bit integer, or return [None] if conversion would be lossy. *)

val to_u8_hlt: t -> u8
(** Convert to an unsigned 8-bit integer, or halt if conversion would be lossy. *)

val to_i8_hlt: t -> i8
(** Convert to a signed 8-bit integer, or halt if conversion would be lossy. *)

val to_u16_hlt: t -> u16
(** Convert to an unsigned 16-bit integer, or halt if conversion would be lossy. *)

val to_i16_hlt: t -> i16
(** Convert to a signed 16-bit integer, or halt if conversion would be lossy. *)

val to_u32_hlt: t -> u32
(** Convert to an unsigned 32-bit integer, or halt if conversion would be lossy. *)

val to_i32_hlt: t -> i32
(** Convert to a signed 32-bit integer, or halt if conversion would be lossy. *)

val to_uns_hlt: t -> uns
(** Convert to an unsigned integer, or halt if conversion would be lossy. *)

val to_int_hlt: t -> sint
(** Convert to a signed integer, or halt if conversion would be lossy. *)

val to_u64_hlt: t -> u64
(** Convert to an unsigned 64-bit integer, or halt if conversion would be lossy. *)

val to_i64_hlt: t -> i64
(** Convert to a signed 8-bit integer, or halt if conversion would be lossy. *)

val to_u128_hlt: t -> u128
(** Convert to an unsigned 128-bit integer, or halt if conversion would be lossy. *)

val to_i128_hlt: t -> i128
(** Convert to a signed 128-bit integer, or halt if conversion would be lossy. *)

val to_u256_hlt: t -> u256
(** Convert to an unsigned 256-bit integer, or halt if conversion would be lossy. *)

val to_i256_hlt: t -> i256
(** Convert to a signed 256-bit integer, or halt if conversion would be lossy. *)

val to_u512_hlt: t -> u512
(** Convert to an unsigned 512-bit integer, or halt if conversion would be lossy. *)

val to_i512_hlt: t -> i512
(** Convert to a signed 512-bit integer, or halt if conversion would be lossy. *)
