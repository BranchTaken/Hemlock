(** 64-bit floating point numeric type. *)

open Rudiments

type t = float

include Identifiable_intf.S with type t := t
include Stringable_intf.S with type t := t

(** Rounding direction. *)
module Dir : sig
  type t =
    | Down    (** Round down (floor). *)
    | Up      (** Round up (ceiling). *)
    | Nearest (** Round to nearest integral. *)
    | Zero    (** Round toward zero. *)

  include Formattable_intf.S_mono with type t := t
end

(** Numeric class. *)
module Class : sig
  type t =
    | Infinite  (** Infinite. *)
    | Nan       (** Not a number. *)
    | Normal    (** Normal. *)
    | Subnormal (** Subnormal. *)
    | Zero      (** Zero. *)

  include Formattable_intf.S_mono with type t := t
end

(** Fractional and integral value results from {!modf}. *)
module Parts : sig
  type outer = t
  type t

  val fractional: t -> outer
  (** Fractional value, i.e. in [(-1.0 .. 1.0)]. *)

  val integral: t -> outer
  (** Integral value, i.e. a whole number. *)

  include Formattable_intf.S_mono with type t := t
end

val of_isize: isize -> t
(** Initialize from signed integer. *)

val to_isize: t -> isize
(** Convert to signed integer. *)

val create: neg:bool -> exponent:isize -> mantissa:usize -> t
(** [create ~neg ~exponent ~mantissa] creates a float, where [-1023 <= exponent
    <= 1024] and [mantissa <= 0xf_ffff_ffff_ffff]. *)

val is_neg: t -> bool
(** [is_neg t] returns [true] if [t] is negative. *)

val exponent: t -> isize
(** [exponent t] returns the exponent of [t], which is in [\[-1023 .. 1024\]].
*)

val mantissa: t -> usize
(** [mantissa t] returns the mantissa of [t], which is in [\[0 ..
    0xf_ffff_ffff_ffff\]]. *)

val m2x: t -> t * isize
(** [m2x t] splits [t] into a normalized fraction [f] and exponent [x], where [t
    = f*2^x]. *)

val f2x: p:isize -> t -> t
(** [f2x ~p t] computes [t*2^~p]. *)

val modf: t -> Parts.t
(** [modf t] splits decomposes [t] into the fractional value and integral value
    which sum to [t].  See {!module:Parts} for further detail. *)

val min_value: t
(** Minimum representable value. *)

val max_value: t
(** Maximum representable value. *)

val one: t
(** Constant value [1.]. *)

val neg_one: t
(** Constant value [-1.]. *)

val nan: t
(** Constant value [nan]; not a number. *)

val inf: t
(** Constant value [inf]; infinity. *)

val neg_inf: t
(** Constant value [neg_inf]; negative infinity. *)

val pi: t
(** Constant value [pi]; ratio of circle circumference to diameter. *)

val ( + ): t -> t -> t
(** Addition. *)

val ( - ): t -> t -> t
(** Subtraction. *)

val ( * ): t -> t -> t
(** Multiplication. *)

val ( / ): t -> t -> t
(** Division. *)

val ( % ): t -> t -> t
(** Modulus. *)

val ( ** ): t -> t -> t
(** [x ** y] returns [x] raised to the [y] power. *)

val ( ~- ): t -> t
(** Unary minus. *)

val ( ~+ ): t -> t
(** Unary plus. *)

val neg: t -> t
(** Negation. *)

val abs: t -> t
(** Absolute value. *)

val copysign: sign:t -> t -> t
(** [copysign ~sign t] returns the composition of the absolute value of [t] and
    the sign of [~sign]. *)

val classify: t -> Class.t
(** Numeric class.  See {!module:Class} for further detail. *)

val is_nan: t -> bool
(** [is_nan t] returns [true] if [t] is not a number. *)

val is_inf: t -> bool
(** [is_inf t] returns [true] if [t] is infinite. *)

val is_fin: t -> bool
(** [is_fin t] returns [true] if [t] is finite. *)

val round: ?dir:Dir.t -> t -> t
(** [round ~dir t] rounds [t] in the specified direction.  [dir] defaults to
    [Nearest]. *)

val min: t -> t -> t
(** [min a b] returns the minimum of [a] and [b]. *)

val max: t -> t -> t
(** [max a b] returns the maximum of [a] and [b]. *)

val ex: t -> t
(** [ex x] computes [e^x]. *)

val exm1: t -> t
(** [exm1 x] computes [e^x - 1]. *)

val lg: t -> t
(** Base 2 logarithm. *)

val ln: t -> t
(** Base e logarithm. *)

val ln1p: t -> t
(** [ln1p x] computes [ln (1 + x)]. *)

val log: t -> t
(** Base 10 logarithm. *)

val pow: p:t -> t -> t
(** [pow ~p t] returns [t] raised to the [~p] power.  Equivalent to [t ** ~p].
*)

val int_pow: p:isize -> t -> t
(** [int_pow ~p t] returns [t] raised to the [~p] power, where [~p] is an
    {!type:isize}.  [int_pow] is typically faster than the equivalent call to
    {!pow}. *)

val lngamma: t -> t
(** [lngamma t] computes the natural log of the {{:
    https://en.wikipedia.org/wiki/Gamma_function} Gamma} function applied to
    [t], accurate to 10 decimal places.  This implementation is based on:

    Pike, M.C., I.D. Hill (1966) Algorithm 291: Logarithm of Gamma
    function \[S14\].  Communications of the ACM 9(9):684. *)

val gamma: t -> t
(** [gamma t] applies the {{: https://en.wikipedia.org/wiki/Gamma_function}
    Gamma} function to [t]. *)

val sqrt: t -> t
(** Square root. *)

val cbrt: t -> t
(** Cube root. *)

val hypot: t -> t -> t
(** [hypot t0 t1] computes the hypotneuse length for a right triangle with sides
    of length [t0] and [t1], i.e [sqrt (t0 * t0 + t1 * t1)]. *)

val sin: t -> t
(** [sin t] computes the sine of [t], where [t] is in radians. *)

val cos: t -> t
(** [cos t] computes the cosine of [t], where [t] is in radians. *)

val tan: t -> t
(** [tan t] computes the tangent of [t], where [t] is in radians. *)

val asin: t -> t
(** [asin t] computes the arc sine of [t] in radians. *)

val acos: t -> t
(** [acos t] computes the arc cosine of [t] in radians. *)

val atan: t -> t
(** [atan t] computes the arc tangent of [t] in radians. *)

val atan2: t -> t -> t
(** [atan2 t0 t1] computes the arc tangent of [t0 / t1], where the signs of [t1]
    and [t0] determine the quadrant of the result. *)

val sinh: t -> t
(** [sinh t] computes the hyperbolic sine of [t]. *)

val cosh: t -> t
(** [cosh t] computes the hyperbolic cosine of [t]. *)

val tanh: t -> t
(** [tanh t] computes the hyperbolic tangent of [t]. *)

(** Float operators. *)
module O : sig
  type nonrec t = t

  include Cmpable_intf.S_mono_infix with type t := t

  val ( + ): t -> t -> t
  (** Addition. *)

  val ( - ): t -> t -> t
  (** Subtraction. *)

  val ( * ): t -> t -> t
  (** Multiplication. *)

  val ( / ): t -> t -> t
  (** Division. *)

  val ( % ): t -> t -> t
  (** Modulus. *)

  val ( ** ): t -> t -> t
  (** [x ** y] returns [x] raised to the [y] power. *)

  val ( ~- ): t -> t
  (** Unary minus. *)

  val ( ~+ ): t -> t
  (** Unary plus. *)

  val neg: t -> t
  (** Negation. *)

  val abs: t -> t
  (** Absolute value. *)
end
