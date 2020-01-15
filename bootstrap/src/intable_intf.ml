(** Intable type functor signature. *)

(** Functor output signature for intable types. *)
module type S = sig
  type t

  val of_int: int -> t
  (** Initialize from signed integer. *)

  val to_int: t -> int
  (** Convert to signed integer. *)
end
