(** Collection of all declared precedences, with automatic assignment of unique indexes. Dominators
    must be declared before any precedences they dominate, which is an intentional semantic
    limitation built into the hocc syntax ([<] relationships but not [>]). *)

open Basis
open! Basis.Rudiments

type t

val empty: t
(** [empty] returns an empty set of precedences. *)

val insert: names:string array -> assoc:(Assoc.t option)
  -> doms:(PrecSet.Index.t, PrecSet.Index.cmper_witness) Ordset.t -> stmt:Parse.nonterm_prec_set
  -> t -> t
(** [insert ~names ~assoc ~doms ~stmt t] creates a [PrecSet.t] with unique index and returns a new
    [t] with the precedence set inserted. *)

val prec_index_of_name: string -> t -> PrecSet.Index.t option
(** [prec_index_of_name s t] returns [Some index] of the precedence with name [s], or [None] if no
    such precedence name exists. *)

val prec_set_of_name: string -> t -> PrecSet.t option
(** [prec_set_of_name s t] returns [Some prec_set] of the precedence set containing name [s], or
    [None] if no such precedence name exists. *)

val prec_of_name: string -> t -> Prec.t option
(** [prec_of_name s t] returns [Some prec] of the precedence with name [s], or [None] if no such
    precedence name exists. *)

val prec_set_of_prec_index: PrecSet.Index.t -> t -> PrecSet.t
(** [prec_set_of_prec_index i t] returns the precedence set with unique index [i]. *)

val length: t -> uns
(** [length t] returns the number of precedences in [t]. *)

val fold_prec_sets: init:'accum -> f:('accum -> PrecSet.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the precedence sets in [t], in increasing index
    order. *)
