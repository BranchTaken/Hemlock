(** Arbitrary-precision binary floating point number. Trailing 0 binary digits in the mantissa are
    normalized away; this preserves accuracy but not explicit precision. Only operations which can
    be performed without loss of precision are supported; e.g. division is intintionally omitted. *)

open Basis
open Basis.Rudiments

type sign =
  | Neg
  | Pos
type t

include IdentifiableIntf.S with type t := t

val create: sign:sign -> exponent:Zint.t -> mantissa:Nat.t -> t
(** [create ~sign ~exponent:(exp_sign, exp) ~mantissa] creates a finite value, where [~sign] is the
    sign, [(exp_sign, exp)] is the exponent sign and magnitude, and [mantissa] is the significant
    binary digits, where for non-zero values the binary point is normalized to immediately follow
    the most significant non-zero bit. *)

val zero: t
(** Constant value [0.]. *)

val one: t
(** Constant value [1.]. *)

val inf: t
(** Constant value [inf]; infinity. *)

val nan: t
(** Constant value [nan]; not a number. *)

val ( + ): t -> t -> t
(** Addition. *)

val ( - ): t -> t -> t
(** Subtraction. *)

val ( * ): t -> t -> t
(** Multiplication. *)

val ( ~- ): t -> t
(** Unary minus. [~-t] is equivalent to [(neg t)]. *)

val ( ~+ ): t -> t
(** Unary plus (no-op). *)

val neg: t -> t
(** Negation. *)

val abs: t -> t
(** Absolute value. *)

val is_fin: t -> bool
(** [is_fin t] returns [true] if [t] is finite. *)

val is_inf: t -> bool
(** [is_inf t] returns [true] if [t] is infinite. *)

val is_nan: t -> bool
(** [is_nan t] returns [true] if [t] is not a number. *)

val to_r64: t -> real
(** Convert to 64-bit real, with possible loss. *)

val to_r64_opt: t -> real option
(** Convert to 64-bit real, or return [None] if conversion would be lossy. *)

val to_r64_hlt: t -> real
(** Convert to 64-bit real, or halt if conversion would be lossy. *)

val to_r32: t -> real
(** Convert to 32-bit real, with possible loss. *)

val to_r32_opt: t -> real option
(** Convert to 32-bit real, or return [None] if conversion would be lossy. *)

val to_r32_hlt: t -> real
(** Convert to 32-bit real, or halt if conversion would be lossy. *)
