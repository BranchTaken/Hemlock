open Rudiments_uint

(** Functor input interface for an integer type with a specific bitwidth. *)
module type I = sig
  type t

  val num_bits: uint
  (** Number of bits in integer representation. *)
end

(** Common subset of functor output signatures for unsigned and signed integer
    types with specific bitwidths. *)
module type S = sig
  type t

  include Identifiable_intf.S with type t := t
  include Cmpable_intf.S_mono_zero with type t := t
  include Floatable_intf.S with type t := t

  val narrow_of_signed: int -> t
  (** Narrow full-width signed integer.  Sign is preserved, but precision may
      be silently lost. *)

  val narrow_of_unsigned: uint -> t
  (** Narrow full-width unsigned integer.  Sign is preserved, but precision may
      be silently lost. *)

  val one: t
  (** Constant value 1. *)

  val min_value: t
  (** Minimum representable value. *)

  val max_value: t
  (** Maximum representable value. *)

  val succ: t -> t
  (** Successor, i.e. [t + 1]. *)

  val pred: t -> t
  (** Predecessor, i.e. [t - 1]. *)

  val bit_and: t -> t -> t
  (** Bitwise and. *)

  val bit_or: t -> t -> t
  (** Bitwise or. *)

  val bit_xor: t -> t -> t
  (** Bitwise xor. *)

  val bit_not: t -> t
  (** Bitwise not. *)

  val bit_sl: t -> uint -> t
  (** Bit shift left. *)

  val bit_usr: t -> uint -> t
  (** Unsigned bit shift right (no sign extension). *)

  val bit_pop: t -> uint
  (** Population count, i.e. number of bits set to 1. *)

  val bit_clz: t -> uint
  (** Count leading zeros. *)

  val bit_ctz: t -> uint
  (** Count trailing zeros. *)

  val is_pow2: t -> bool
  (** [is_pow2 t] returns [true] if [t] is a power of 2, [false] otherwise. *)

  val floor_pow2: t -> t
  (** [floor_pow2 t] returns the largest power of 2 less than or equal to [t].
      *)

  val ceil_pow2: t -> t
  (** [ceil_pow2 t] returns the smallest power of 2 greater than or equal to
      [t]. *)

  val floor_lg: t -> t
  (** [floor_lg t] returns the base 2 logarithm of [t], rounded down to the
      nearest integer. *)

  val ceil_lg: t -> t
  (** [ceil_lg t] returns the base 2 logarithm of [t], rounded up to the
      nearest integer. *)

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

  val ( // ): t -> t -> float
  (** [x // y] performs floating point division on float-converted [x] and [y].
      *)

  val min: t -> t -> t
  (** [min a b] returns the minimum of [a] and [b]. *)

  val max: t -> t -> t
  (** [max a b] returns the maximum of [a] and [b]. *)
end

(** Functor output signature for an unsigned integer type with a specific
    bitwidth. *)
module type S_u = sig
  include S
end

(** Functor output signature for a signed integer type with a specific bitwidth. 
    *)
module type S_i = sig
  include S

  val neg_one: t
  (** Constant value -1. *)

  val bit_ssr: t -> uint -> t
  (** Signed bit shift right (sign extension). *)

  val ( ~- ): t -> t
  (** Unary minus. *)

  val ( ~+ ): t -> t
  (** Unary plus. *)

  val neg: t -> t
  (** Negation. *)

  val abs: t -> t
  (** Absolute value. *)
end
