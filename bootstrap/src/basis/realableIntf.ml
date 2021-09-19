(** Functor output signature for types that can convert to/from {!type:real}. *)
module type S = sig
  type t
  (** Realable type. *)

  val of_real: float -> t
  (** [of_real r] converts [r] to {!type:t}. *)

  val to_real: t -> float
  (** [to_real t] converts [t] to {!type:real}. *)
end
