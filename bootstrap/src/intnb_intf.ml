open Rudiments_uint

module type I = sig
  type t
  val num_bits: uint
end

module type S = sig
  type t

  include Identifiable_intf.S with type t := t
  include Cmpable_intf.S_zero with type t := t
  include Floatable_intf.S with type t := t

  val narrow_of_signed: int -> t
  val narrow_of_unsigned: uint -> t

  val one: t

  val min_value: t
  val max_value: t

  val succ: t -> t
  val pred: t -> t

  val bit_and: t -> t -> t
  val bit_or: t -> t -> t
  val bit_xor: t -> t -> t
  val bit_not: t -> t
  val bit_sl: t -> uint -> t
  val bit_usr: t -> uint -> t
  val bit_pop: t -> uint
  val bit_clz: t -> uint
  val bit_ctz: t -> uint

  val is_pow2: t -> bool
  val floor_pow2: t -> t
  val ceil_pow2: t -> t
  val floor_lg: t -> t
  val ceil_lg: t -> t

  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val ( % ): t -> t -> t
  val ( ** ): t -> t -> t
  val ( // ): t -> t -> float

  val min: t -> t -> t
  val max: t -> t -> t
end

module type S_u = sig
  include S
end

module type S_i = sig
  include S

  val neg_one: t

  val bit_ssr: t -> uint -> t

  val ( ~- ): t -> t
  val ( ~+ ): t -> t
  val neg: t -> t
  val abs: t -> t
end
