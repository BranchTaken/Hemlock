(** Functor output signature for types that can convert to/from {!type:float}.
    *)
module type S = sig
  type t
  (** Floatable type. *)

  val of_float: float -> t
  (** [of_float f] converts [f] to {!type:t}. *)

  val to_float: t -> float
  (** [to_float t] converts [t] to {!type:float}. *)
end
