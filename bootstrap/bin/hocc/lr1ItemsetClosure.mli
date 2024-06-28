(** LR(1) item set closure. *)

open Basis
open! Basis.Rudiments

(* Isomorphic with `State.Index`. *)
module Index = Uns

module Action : sig
  type t =
    | ShiftPrefix of Lr1Itemset.t (** Shift, transition to an intermediate state. *)
    | ShiftAccept of Lr1Itemset.t (** Shift, transition to a successful parse state. *)
    | Reduce of Prod.Index.t (** Reduce. *)

  include IdentifiableIntf.S with type t := t
end

module Actionset: sig
  type t = (Action.t, Action.cmper_witness) Ordset.t
end

type t = {
  index: Index.t;
  (** Unique LR(1) item set closure index. *)

  kernel: Lr1Itemset.t;
  (** Kernel items, i.e. items which are in the set prior to closure. *)

  added: Lr1Itemset.t;
  (** Added items, i.e. items which are added during closure. *)
}

include IdentifiableIntf.S with type t := t

val init: Symbols.t -> index:Index.t -> Lr1Itemset.t -> t
(** [init symbols ~index lr1itemset] creates the closure of the kernel represented by [lr1itemset],
    with index set to [index]. *)

val remerge: Symbols.t -> (Index.t, Index.t, Index.cmper_witness) Ordmap.t -> t -> t -> t
(** [remerge symbols remergeable_index_map t0 t1] re-merges the kernels of [t0] and [t1] creates the
    closure of the merged kernel. *)

val reindex: (Index.t, Index.t, Index.cmper_witness) Ordmap.t -> t -> t
(** [reindex index_map t] creates an LR(1) item set closure with all LR(1) item set closure indexes
    translated according to [index_map], where keys are the original indexes, and values are the
    reindexed indexes. *)

val merge: Symbols.t -> Lr1Itemset.t -> t -> bool * t
(** [merge symbols lr1itemset t] merges the kernel represented by [lr1itemset] into [t]'s kernel and
    creates the closure of the merged kernel. The boolean result indicates whether items were merged
    into the kernel. *)

val next: t -> (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t
(** [next t] returns the set of symbol indexes that may appear next, i.e. the symbol indexes
    corresponding to the symbols for which [goto] returns a non-empty set. *)

val goto: Symbol.t -> t -> Lr1Itemset.t
(** [goto symbol t] computes the kernel of the goto set reachable from [t], given [symbol]. *)

val actions: Symbols.t -> t -> (Symbol.Index.t, Actionset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [actions symbols t] computes the map of per symbol actions for [t]. *)

val gotos: Symbols.t -> t -> (Symbol.Index.t, Lr1Itemset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [gotos symbols t] computes the map of per non-terminal symbol gotos for [t]. *)

val lhs_symbol_indexes: t -> (Symbol.Index.t, (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t,
  Symbol.Index.cmper_witness) Ordmap.t
(** [lhs_symbol_indexes t] returns a map of all LHS symbols in [t] to their corresponding items'
    follow sets. *)

val kernel_of_leftmost: symbol_index:Symbol.Index.t -> lhs_index:Symbol.Index.t -> t -> Lr1Itemset.t
(** [kernel_of_leftmost ~symbol_index ~lhs_index] returns the transitive closure of the kernel items
    with [lhs_index] just past the dot and [symbol_index] in the follow set. *)

module LeftmostCache : sig
  type outer = t
  type t

  val empty: t

  val kernel_of_leftmost: symbol_index:Symbol.Index.t -> lhs_index:Symbol.Index.t -> outer -> t
    -> Lr1Itemset.t * t
    (** [kernel_of_leftmost ~symbol_index ~lhs_index lr1itemset] returns the transitive closure of
        the kernel items with [lhs_index] just past the dot and [symbol_index] in the follow set, as
        well as an updated [t] with the result memoized. *)
end

val fold_until: init:'accum -> f:('accum -> Lr1Item.t -> 'accum * bool) -> t -> 'accum
(** [fold_until ~init ~f t] folds over all kernel and added items in [t], continuing until [f]
    returns [(accum, true)], or until folding is complete. *)

val fold: init:'accum -> f:('accum -> Lr1Item.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over all kernel and added items in [t]. *)
