(** Collection of all productions, with automatic assignment of unique indexes. Special
    initialization code typically inserts synthetic productions that wrap start symbols. *)

open! Basis
open! Basis.Rudiments

type t

module Builder : sig
  type outer = t
  type t

  val empty: t
  (** [empty] returns an empty productions builder. *)

  val insert: lhs_index:Symbol.Index.t -> rhs_indexes:Symbol.Index.t array -> prec:Prec.t option
    -> stmt:Parse.nonterm_prod option -> callback:Callback.t -> t -> Prod.t * t
  (** [insert ~lhs_index ~rhs_indexes ~prec ~stmt ~callback t] creates a [Prod.t] with unique index
      and returns both the production and a new [t] with the production inserted. *)

  val build: t -> outer
  (** [build t] builds a [Prods.t]. *)
end

val use_prec: Prod.Index.t -> t -> t
(** [use_prec prod_index t] returns a derivative of [t] with the precedence for the production
    corresponding to [prod_index] marked as useful. *)

val length: t -> uns
(** [length t] returns the number of productions in [t]. *)

val prod_of_prod_index: Prod.Index.t -> t -> Prod.t
(** [prod_of_prod_index i t] returns the production with unique index [i]. *)

val fold: init:'accum -> f:('accum -> Prod.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the productions in [t], in increasing index order.
*)

val src_fmt: Precs.t -> Symbols.t -> Prod.t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs production in hocc syntax. *)
