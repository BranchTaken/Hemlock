module type I = sig
  type t
  val cmp: t -> t -> Cmp.t
end

module type I_zero = sig
  include I
  val zero: t
end

module type S = sig
  include I
  val ( >= ): t -> t -> bool
  val ( <= ): t -> t -> bool
  val ( = ): t -> t -> bool
  val ( > ): t -> t -> bool
  val ( < ): t -> t -> bool
  val ( <> ): t -> t -> bool
  val ascending: t -> t -> Cmp.t
  val descending: t -> t -> Cmp.t
  val clamp: t -> min:t -> max:t -> t
  val between: t -> low:t -> high:t -> bool
end

module type S_zero = sig
  include I_zero
  val is_positive: t -> bool
  val is_non_negative: t -> bool
  val is_negative: t -> bool
  val is_non_positive: t -> bool
  val sign: t -> Sign.t
end

(* Polymorphic. *)

module type I_poly = sig
  type 'a t
  val cmp: 'a t -> 'a t -> Cmp.t
end

module type S_poly = sig
  include I_poly
  val ( >= ): 'a t -> 'a t -> bool
  val ( <= ): 'a t -> 'a t -> bool
  val ( = ): 'a t -> 'a t -> bool
  val ( > ): 'a t -> 'a t -> bool
  val ( < ): 'a t -> 'a t -> bool
  val ( <> ): 'a t -> 'a t -> bool
  val ascending: 'a t -> 'a t -> Cmp.t
  val descending: 'a t -> 'a t -> Cmp.t
  val clamp: 'a t -> min:'a t -> max:'a t -> 'a t
  val between: 'a t -> low:'a t -> high:'a t -> bool
end
