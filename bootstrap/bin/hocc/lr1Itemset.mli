(** Set of LR(1) items. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val fmt_hr: Symbols.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols ~alt ~width t formatter] formats a human-readable representation of [t]. If
    [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

module Seq : sig
  type container = t

  include SeqIntf.SMonoDef with type elm = Lr1Item.t

  val init: container -> t
end

val empty: t
(** [empty] returns an empty LR(1) item set. *)

val singleton: Lr1Item.t -> t
(** [singleton lr1item] returns an LR(1) item set containing only [lr1item]. *)

val length: t -> uns
(** [length t] returns the number of LR(1) items in [t]. *)

val is_start: t -> bool
(** [is_start t] returns true if [t] corresponds to a start state. In the context of parser
    generation, this is true by construction iff one or more of the contained LR(1) items has a dot
    position of 0. Other LR(1) items can (and often do) exist in a grammar, but they are never
    inserted into an LR(1) item set. *)

val start_symbol_index: t -> Symbol.Index.t
(** [start_symbol_index t] returns the start symbol index of [t], where [t] must be a start state.
*)

val is_empty: t -> bool
(** [is_empty t] returns true if [t] contains no LR(1) items. *)

val mem: Lr1Item.t -> t -> bool
(** [mem lr1item t] returns true if [t] contains [lr1item]. *)

val choose: t -> Lr1Item.t option
(** [choose t] returns an arbitrary LR(1) item in [t] if the set is non-empty, [None] otherwise. *)

val get: Lr1Item.t -> t -> Lr1Item.t option
(** [get lr1item t] returns the LR(1) item in [t] with LR(0) core matching that of [lr1item] and
    follow set containing a non-strict superset of that of [lr1item] if present, [None] otherwise.
*)

val insert: Lr1Item.t -> t -> t
(** [insert lr1item t] creates an LR(1) item set equivalent to [t] with [lr1item] inserted, or
    returns [t] if [lr1item] is already present in [t]. *)

val insert_hlt: Lr1Item.t -> t -> t
(** [insert lr1item t] creates an LR(1) item set equivalent to [t] with [lr1item] inserted, or halts
    if [lr1item] is already present in [t]. *)

val remove: Lr1Item.t -> t -> t
(** [remove lr1item t] creates an LR(1) item set equivalent to [t] with [lr1item] removed, or
    returns [t] if [lr1item] is not present in [t]. *)

val union: t -> t -> t
(** [union t0 t1] creates an LR(1) item set containing all the items in [t0] or [t1]. *)

val inter: t -> t -> t
(** [inter t0 t1] creates an LR(1) item set containing all the items in [t0] and [t1]. *)

val diff: t -> t -> t
(** [diff t0 t1] creates an LR(1) item set containing the items in [t0] that are not in [t1]. *)

val fold_until: init:'accum -> f:('accum -> Lr1Item.t -> 'accum * bool) -> t -> 'accum
(** [fold_until ~init ~f t] folds [t] using [init] as the initial accumulator value, continuing
    until [f] returns [accum, true], or until folding is complete if [f] always returns [accum,
    false]. *)

val fold: init:'accum -> f:('accum -> Lr1Item.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds [t] using [init] as the initial accumulator value. *)

val core: t -> Lr0Itemset.t
(** [core t] returns the LR(0) item set corresponding to [t], i.e. [t] with no lookahead. *)

val is_accept: t -> bool
(** [is_accept t] returns true iff [t] contains only LR(1) items which are compatible with an accept
    state. *)

val compat_lr1: t -> t -> bool
(** [compat_lr1 t0 t1] determines if isocores [t0] and [t1] are identical, which is the basis
    of the canonical LR(1) algorithm. *)

val compat_pgm1: t -> t -> bool
(** [compat_pgm1 t0 t1] determines if isocores [t0] and [t1] are weakly compatible, as defined by
    the Pager(1977) algorithm, and as refined by Menhir to prevent phantom conflicts accompanying
    actual conflicts. This is the basis of the PGM(1) algorithm. *)

val compat_lalr1: t -> t -> bool
(** [compat_lalr t0 t1] determines if isocore [t0] has the same LR(0) kernel as [t1], which is the
    basis of the LALR(1) algorithm. This is trivially true for all isocores. *)
