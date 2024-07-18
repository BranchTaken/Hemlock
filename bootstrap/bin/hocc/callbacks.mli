(** Collection of all reduction callbacks, with automatic assignment of unique indexes. *)
open! Basis
open! Basis.Rudiments

type t

val empty: t
(** [empty] returns an empty set of reduction callbacks. *)

val insert: lhs:Symbols.info -> rhs:Callback.Params.t -> code:Parse.nonterm_code option -> t
  -> Callback.t * t
(** [insert ~lhs ~rhs ~code t] creates a [Callback.t] with unique index and returns both the
    reduction callback and a new [t] with the reduction callback inserted. *)

val length: t -> uns
(** [length t] returns the number of reduction callbacks in [t]. *)

val fold: init:'accum -> f:('accum -> Callback.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the reduction callbacks in [t], in increasing index
    order.
*)
