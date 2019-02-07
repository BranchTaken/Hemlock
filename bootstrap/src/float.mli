type t = float [@@deriving compare]

include Identifiable_intf.S with type t := t
include Intable_intf.S with type t := t

module Dir: sig
  type t =
  | Down
  | Up
  | Nearest
  | Zero
  [@@deriving sexp]
end

module Class: sig
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  [@@deriving sexp]
end

module Parts: sig
  type outer = t
  type t [@@deriving sexp]

  val fractional: t -> outer
  val integral: t -> outer
end

val create: neg:bool -> exponent:int -> mantissa:int -> t
val is_neg: t -> bool
val exponent: t -> int
val mantissa: t -> int
val m2x: t -> t * int
val f2x: t -> int -> t
val modf: t -> Parts.t

val min_value: t
val max_value: t

val one: t
val neg_one: t
val nan: t
val inf: t
val neg_inf: t
val pi: t

val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( * ): t -> t -> t
val ( / ): t -> t -> t
val ( % ): t -> t -> t
val ( ** ): t -> t -> t
val ( ~- ): t -> t
val ( ~+ ): t -> t
val neg: t -> t
val abs: t -> t
val copysign: t -> t -> t

val classify: t -> Class.t
val is_nan: t -> bool
val is_inf: t -> bool
val is_fin: t -> bool

val round: ?dir:Dir.t -> t -> t

val min: t -> t -> t
val max: t -> t -> t

val ex: t -> t
val exm1: t -> t

val lg: t -> t
val ln: t -> t
val ln1p: t -> t
val log: t -> t

val pow: t -> t -> t (* Same as ( ** ). *)
val int_pow: t -> int -> t

val lngamma: t -> t
(** Compute the natural log of Gamma(x), accurate to 10 decimal places.
    This implementation is based on:

    Pike, M.C., I.D. Hill (1966) Algorithm 291: Logarithm of Gamma
    function [S14].  Communications of the ACM 9(9):684. *)
val gamma: t -> t

val sqrt: t -> t
val cbrt: t -> t
val hypot: t -> t -> t

val sin: t -> t
val cos: t -> t
val tan: t -> t
val asin: t -> t
val acos: t -> t
val atan: t -> t
val atan2: t -> t -> t

val sinh: t -> t
val cosh: t -> t
val tanh: t -> t
