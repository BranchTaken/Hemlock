module type I = sig
  type t
  val cmp: t -> t -> Cmp.t
end

module type S_eq = sig
  type t
  val ( = ): t -> t -> bool
  val ( <> ): t -> t -> bool
end

module type S_rel = sig
  type t
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
