(** Functor interfaces and signatures for integers of specific bitwidth. *)

include RudimentsInt0
type real = float

(** Functor input interface for an integer type with a specific bitwidth. *)
module type I = sig
  type t

  val bit_length: uns
  (** Number of bits in integer representation. *)
end

(** Functor input interface required to derive functions on an integer type with
    a specific bitwidth. *)
module type IDerived = sig
  type t

  include I with type t := t
  include CmpableIntf.IMonoZero with type t := t

  val one: t
  (** Constant value 1. *)

  val of_uns: uns -> t
  (** Initialize from full-width unsigned integer, with possible loss. *)

  val ( + ): t -> t -> t
  (** Addition. *)

  val ( - ): t -> t -> t
  (** Subtraction. *)

  val bit_and: t -> t -> t
  (** Bitwise and. *)

  val bit_sl: shift:uns -> t -> t
  (** Bit shift left. *)

  val bit_clz: t -> uns
  (** Count leading zeros. *)
end

module type SDerived = sig
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
    types, but without [min_value] and [max_value], which are not always usable
    with variable-bitwidth types. *)
module type SLimitless = sig
  type t

  include IdentifiableIntf.S with type t := t
  include StringableIntf.S with type t := t
  include CmpableIntf.SMonoZero with type t := t
  include RealableIntf.S with type t := t

  val pp_b: Format.formatter -> t -> unit
  (** [pp_b ppf t] prints a binary representation of [t] to the pretty printing
      formatter, [ppf]. This function is intended for use with the [%a] format
      specifier to {!Format.printf}.*)

  val pp_o: Format.formatter -> t -> unit
  (** [pp_o ppf t] prints an octal representation of [t] to the pretty printing
      formatter, [ppf]. This function is intended for use with the [%a] format
      specifier to {!Format.printf}.*)

  val pp_x: Format.formatter -> t -> unit
  (** [pp_x ppf t] prints a hexadecimal representation of [t] to the pretty
      printing formatter, [ppf]. This function is intended for use with the [%a]
      format specifier to {!Format.printf}.*)

  val one: t
  (** Constant value 1. *)

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

  val bit_sl: shift:uns -> t -> t
  (** Bit shift left. *)

  val bit_usr: shift:uns -> t -> t
  (** Unsigned bit shift right (no sign extension). *)

  val bit_ssr: shift:uns -> t -> t
  (** Signed bit shift right (sign extension). *)

  val bit_pop: t -> uns
  (** Population count, i.e. number of bits set to 1. *)

  val bit_clz: t -> uns
  (** Count leading zeros. *)

  val bit_ctz: t -> uns
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

  val ( // ): t -> t -> real
  (** [x // y] performs real division on real-converted [x] and [y]. *)

  include SDerived with type t := t
end

(** Common subset of functor output signatures for unsigned and signed integer
    types with specific bitwidths. *)
module type S = sig
  include SLimitless

  val min_value: t
  (** Minimum representable value. *)

  val max_value: t
  (** Maximum representable value. *)
end

(** Functor output signature for narrowing functions. These functions are only
    needed by integer types based on the default integer types, and are a
    bootstrapping artifact. *)
module type SNarrow = sig
  type t

  val narrow_of_signed: sint -> t
  (** Narrow full-width signed integer. Sign is preserved, but precision may be
      silently lost. *)

  val narrow_of_unsigned: uns -> t
  (** Narrow full-width unsigned integer. Sign is preserved, but precision may
      be silently lost. *)
end

(** Functor output signature for an unsigned integer type with a specific
    bitwidth. *)
module type SU = sig
  type t

  include S with type t := t
  include SNarrow with type t := t
end

(** Functor output signature for functions that exist only for signed integers.
*)
module type SSigned = sig
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
module type SI = sig
  type t

  include S with type t := t
  include SNarrow with type t := t
  include SSigned with type t := t
end
