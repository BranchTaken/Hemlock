module type I = sig
  type t
  val num_bits: int
end

module type S = sig
  type t [@@deriving compare]

  include Cmpable_intf.I_zero with type t := t
  include Cmpable_intf.S_rel with type t := t
  include Cmpable_intf.S_zero with type t := t

  include Floatable_intf.S with type t := t
  include Stringable_intf.S with type t := t
  include Sexpable_intf.S with type t := t

  val narrow_of_signed: t -> t
  val narrow_of_unsigned: t -> t

  val min_value: t
  val max_value: t

  val one: t

  val succ: t -> t
  val pred: t -> t

  val bit_and: t -> t -> t
  val bit_or: t -> t -> t
  val bit_xor: t -> t -> t
  val bit_not: t -> t
  val bit_sl: t -> t -> t
  val bit_usr: t -> t -> t
  val bit_pop: t -> t
  val bit_clz: t -> t
  val bit_ctz: t -> t

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

  val bit_ssr: t -> t -> t

  val ( ~- ): t -> t
  val ( ~+ ): t -> t
  val neg: t -> t
  val abs: t -> t
end
