(** Collection of all declared precedences, with automatic assignment of unique indexes. Dominators
    must be declared before any precedences they dominate, which is an intentional semantic
    limitation built into the hocc syntax ([<] relationships but not [>]). *)

open Basis
open! Basis.Rudiments

type t

val empty: t
(** [empty] returns an empty set of precedences. *)

val insert: name:string -> assoc:(Assoc.t option)
  -> doms:(Prec.Index.t, Prec.Index.cmper_witness) Ordset.t -> stmt:Parse.nonterm_prec -> t -> t
(** [insert ~name ~assoc ~doms ~stmt t] creates a [Prec.t] with unique index and returns a new [t]
    with the production inserted. *)

val prec_index_of_name: string -> t -> Prec.Index.t option
(** [prec_index_of_name s t] returns [Some index] of the precedence with name [s], or [None] if no
    such precedence name exists. *)

val prec_of_name: string -> t -> Prec.t option
(** [prec_of_name s t] returns [Some prec] of the precedenc with name [s], or [None] if no such
    precedence name exists. *)

val prec_of_prec_index: Prec.Index.t -> t -> Prec.t
(** [prec_of_prec_index i t] returns the precedence with unique index [i]. *)

val length: t -> uns
(** [length t] returns the number of precedences in [t]. *)

val fold: init:'accum -> f:('accum -> Prec.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the precedences in [t], in increasing index order.
*)
