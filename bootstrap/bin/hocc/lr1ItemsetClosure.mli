(** LR(1) item set closure. *)

open Basis
open! Basis.Rudiments

(* Isomorphic with `State.Index`. *)
module Index = Uns

type t = {
  index: Index.t;
  (** Unique LR(1) item set closure index. *)

  kernel: Lr1Itemset.t;
  (** Kernel items, i.e. items which are in the set prior to closure. *)

  added: Lr1Itemset.t lazy_t;
  (** Added items, i.e. items which are added during closure. *)
}

include IdentifiableIntf.S with type t := t

val init: Symbols.t -> index:Index.t -> Lr1Itemset.t -> t
(** [init symbols ~index lr1itemset] creates the closure of the kernel represented by [lr1itemset],
    with index set to [index]. *)

val added: t -> Lr1Itemset.t
(** [added t] returns added items, i.e. items which are added during closure. *)

val remerge: Symbols.t -> t -> t -> t
(** [remerge symbols t0 t1] re-merges the kernel of [t0] into [t1], where [t0] has a higher index
    than [t1], and creates the closure of the merged kernel. *)

val reindex: StateIndexMap.t -> t -> t
(** [reindex state_index_map t] creates an LR(1) item set closure with all LR(1) item set closure
    indexes translated according to [state_index_map]. *)

val merge: Symbols.t -> Lr1Itemset.t -> t -> bool * t
(** [merge symbols lr1itemset t] merges the kernel represented by [lr1itemset] into [t]'s kernel and
    creates the closure of the merged kernel. The boolean result indicates whether items were merged
    into the kernel. *)

val fold_next: Symbols.t -> init:'accum -> f:('accum -> (Symbol.Index.t * Lr1Itemset.t) -> 'accum)
  -> t -> 'accum
(** [fold_next symbols ~init ~f t] folds over the set of symbol indexes that may appear next, along
    with their goto sets. *)

val token_gotos: Symbols.t -> t
  -> (Symbol.Index.t, Lr1Itemset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [token_gotos symbols t] computes the map of per terminal symbol gotos for [t]. *)

val nonterm_gotos: Symbols.t -> t
  -> (Symbol.Index.t, Lr1Itemset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [nonterm_gotos symbols t] computes the map of per non-terminal symbol gotos for [t]. *)

val fold_until: init:'accum -> f:('accum -> Lr1Item.t -> 'accum * bool) -> t -> 'accum
(** [fold_until ~init ~f t] folds over all kernel and added items in [t], continuing until [f]
    returns [(accum, true)], or until folding is complete. *)

val fold: init:'accum -> f:('accum -> Lr1Item.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over all kernel and added items in [t]. *)

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
