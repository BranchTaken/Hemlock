(** Collection of all reductions, with automatic assignment of unique indexes. *)
open! Basis
open! Basis.Rudiments

type t

val empty: t
(** [empty] returns an empty set of reductions. *)

val insert: lhs:QualifiedType.t -> rhs:Reduction.Params.t -> code:Parse.code option -> t
  -> Reduction.t * t
(** [insert ~lhs ~rhs ~code t] creates a [Reduction.t] with unique index and returns both the
    reduction and a new [t] with the reduction inserted. *)

val length: t -> uns
(** [length t] returns the number of reductions in [t]. *)

val fold: init:'accum -> f:('accum -> Reduction.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the reductions in [t], in increasing index order.
*)
