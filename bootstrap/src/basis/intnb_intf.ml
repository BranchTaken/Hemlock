(** Functor interfaces and signatures for integers of specific bitwidth. *)

include Rudiments_int0

(** Functor input interface for an integer type with a specific bitwidth. *)
module type I = sig
  type t

  val num_bits: usize
  (** Number of bits in integer representation. *)
end

(** Functor input interface required to derive functions on an integer type with
    a specific bitwidth. *)
module type I_derived = sig
  type t

  include I with type t := t
  include Cmpable_intf.I_mono_zero with type t := t

  val one: t
  (** Constant value 1. *)

  val of_usize: usize -> t
  (** Initialize from full-width unsigned integer, with possible loss. *)

  val ( + ): t -> t -> t
  (** Addition. *)

  val ( - ): t -> t -> t
  (** Subtraction. *)

  val bit_and: t -> t -> t
  (** Bitwise and. *)

  val bit_sl: shift:usize -> t -> t
  (** Bit shift left. *)

  val bit_clz: t -> usize
  (** Count leading zeros. *)
end

module type S_derived = sig
  type t

  val is_pow2: t -> bool
  (** [is_pow2 t] returns [true] if [t] is a power of 2, [false] otherwise. *)

  val floor_pow2: t -> t
  (** [floor_pow2 t] returns the largest power of 2 less than or equal to [t],
      or halts if [t] is less than 1. *)

  val ceil_pow2: t -> t
  (** [ceil_pow2 t] returns the smallest power of 2 greater than or equal to
      [t], or halts if [t] is less than 1. *)

  val floor_lg: t -> t
  (** [floor_lg t] returns the base 2 logarithm of [t], rounded down to the
      nearest integer, or halts if [t] less than 1. *)

  val ceil_lg: t -> t
  (** [ceil_lg t] returns the base 2 logarithm of [t], rounded up to the nearest
      integer, or halts if [t] is less than 1. *)

  val min: t -> t -> t
  (** [min a b] returns the minimum of [a] and [b]. *)

  val max: t -> t -> t
  (** [max a b] returns the maximum of [a] and [b]. *)
end

(** Common subset of functor output signatures for unsigned and signed integer
    types with specific bitwidths. *)
module type S = sig
  type t

  include Identifiable_intf.S with type t := t
  include Stringable_intf.S with type t := t
  include Cmpable_intf.S_mono_zero with type t := t
  include Floatable_intf.S with type t := t

  val pp_x: Format.formatter -> t -> unit
  (** [pp_x ppf t] prints a hexadecimal representation of [t] to the pretty
      printing formatter, [ppf].  This function is intended for use with the
      [%a] format specifier to {!Format.printf}.*)

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

  val bit_sl: shift:usize -> t -> t
  (** Bit shift left. *)

  val bit_usr: shift:usize -> t -> t
  (** Unsigned bit shift right (no sign extension). *)

  val bit_ssr: shift:usize -> t -> t
  (** Signed bit shift right (sign extension). *)

  val bit_pop: t -> usize
  (** Population count, i.e. number of bits set to 1. *)

  val bit_clz: t -> usize
  (** Count leading zeros. *)

  val bit_ctz: t -> usize
  (** Count trailing zeros. *)

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

  include S_derived with type t := t
end

(** Functor output signature for narrowing functions.  These functions are only
    needed by integer types based on the default integer types, and are a
    bootstrapping artifact. *)
module type S_narrow = sig
  type t

  val narrow_of_signed: isize -> t
  (** Narrow full-width signed integer.  Sign is preserved, but precision may be
      silently lost. *)

  val narrow_of_unsigned: usize -> t
  (** Narrow full-width unsigned integer.  Sign is preserved, but precision may
      be silently lost. *)
end

(** Functor output signature for an unsigned integer type with a specific
    bitwidth. *)
module type S_u = sig
  type t

  include S with type t := t
  include S_narrow with type t := t
end

(** Functor output signature for functions that exist only for signed integers.
*)
module type S_signed = sig
  type t

  val neg_one: t
  (** Constant value -1. *)

  val ( ~- ): t -> t
  (** Unary minus. *)

  val ( ~+ ): t -> t
  (** Unary plus. *)

  val neg: t -> t
  (** Negation. *)

  val abs: t -> t
  (** Absolute value. *)
end

(** Functor output signature for a signed integer type with a specific bitwidth.
*)
module type S_i = sig
  type t

  include S with type t := t
  include S_narrow with type t := t
  include S_signed with type t := t
end
