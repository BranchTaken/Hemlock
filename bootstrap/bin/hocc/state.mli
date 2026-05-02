(** Characteristic finite state machine (CFSM) state. *)

open Basis
open Basis.Rudiments

(* Isomorphic with `Lr1ItemsetClosure.Index`. *)
module Index = StateIndex

module Action : sig
  type t =
    | ShiftPrefix of Index.t (** Shift, transition to an intermediate state. *)
    | ShiftAccept of Index.t (** Shift, transition to a successful parse state. *)
    | Reduce of Prod.Index.t (** Reduce. *)

  include IdentifiableIntf.S with type t := t

  val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** Formatter which outputs action in human-readable form. *)
end

module ActionSet : sig
  type t

  val length: t -> uns
  (** [length t] returns the number of elements in [t]. *)

  val choose_hlt: t -> Action.t
  (** [choose_hlt t] returns an arbitrary member of [t] if the set is non-empty, halts otherwise. *)

  val equal: t -> t -> bool
  (** [equal t0 t1] returns [true] if [t0] and [t1] contain identical sets of elements, [false]
      otherwise. *)

  val fold: init:'accum -> f:('accum -> Action.t -> 'accum) -> t -> 'accum
  (** [fold ~init ~f t] folds [t] from left to right, using [init] as the initial accumulator value.
  *)

  val for_any: f:(Action.t -> bool) -> t -> bool
  (** [for_any ~f t] iterates from left to right over [t] and returns [true] if any invocation of
      [f] returns [true]. *)

  val for_all: f:(Action.t -> bool) -> t -> bool
  (** [for_all ~f t] iterates from left to right over [t] and returns [true] if all invocations of
      [f] return [true]. *)

  val fold2_until: init:'accum ->
    f:('accum -> Action.t option -> Action.t option -> 'accum * bool) -> t -> t -> 'accum
  (** [fold2_until ~init ~f t0 t1] folds over the union of t0 and t1 from left to right and calls
      [~f accum elm0_opt elm1_opt] once for each element in the union such that if the element is
      absent from one of the input sets, the corresponding parameter is [None]. Note that shift
      actions are paired even if their destinations differ. Folding terminates early if [~f] returns
      [(_, true)]. *)
end

type t = {
  statenub: StateNub.t;
  (** State nub, which contains the LR(1) item set closure and inadequacy attributions. *)

  resolvers: Resolvers.t;
  (** Sets of associativities/precedences that were useful for conflict resolution. *)

  actions: (Symbol.Index.t, ActionSet.t, Symbol.Index.cmper_witness) Ordmap.t;
  (** Per symbol action sets (i.e. potentially ambiguous) with conflict resolution if the [~resolve]
      parameter to [init] was true, as well as reindexing and unreachable action filtering depending
      on phase of automaton construction. *)

  gotos: (Symbol.Index.t, Lr1ItemsetClosure.Index.t, Symbol.Index.cmper_witness) Ordmap.t;
  (** Per symbol gotos, which are consulted during reduction state transitions, with unreachable
      goto filtering depending on phase of automaton construction. *)
}

include IdentifiableIntf.S with type t := t

val init: resolve:bool -> Precs.t -> Symbols.t -> Prods.t -> Isocores.t
  -> gotonub_of_statenub_goto:(StateNub.t -> Lr1Itemset.t -> GotoNub.t) -> StateNub.t -> t
(** [init ~resolve precs symbols prods isocores ~gotonub_of_statenub_goto statenub] creates a state
    based on [statenub]. *)

val remerge: Symbols.t -> t -> t -> t
(** [remerge symbols t0 t1] re-merges state [t0] into [t1], where [t0] has a higher index than [t1].
*)

val reindex: StateIndexMap.t ->  (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t option -> t
  -> t
(** [reindex state_index_map reachable_action_symbols_opt t] creates a state with all LR(1) item set
    closure, state nub, and state indexes translated according to [state_index_map], and reachable
    actions specified by [reachable_action_symbols_opt] if there are any unreachable actions. *)

val index: t -> Index.t
(** [index t] returns the index of the contained unique LR(1) item set closure. *)

val is_start: t -> bool
(** [is_start t] returns true if [t] is a start state. *)

val start_symbol_index: t -> Symbol.Index.t
(** [start_symbol_index t] returns the start symbol index of [t], where [t] must be a start state.
*)

val has_pseudo_end_conflict: t -> bool
(** [has_pseudo_end_conflict t] returns true if the state conflicts on the pseudo-end (⊥) symbol. *)

val has_conflict_attribs: resolve:bool -> Precs.t -> Symbols.t -> Prods.t -> t -> bool
(** [has_conflict_attribs ~resolve precs symbols prods t] returns true iff there are conflict
    attributions, i.e. per symbol conflict attributions. If [resolve] is true, omit conflicts that
    cannot result in inadequacy in the context of conflict resolution (i.e. conflicts that resolve
    to shift). The pseudo-end (⊥) symbol is omitted, because this function is used for attributing
    conflict attributions, and conflicting actions on ⊥ are a special case to which conflict
    attributions do not apply. *)

val conflict_attribs: resolve:bool -> Precs.t -> Symbols.t -> Prods.t -> t -> Attribs.t
(** [conflict_attribs ~resolve precs symbols prods t] returns conflict attributions. If [resolve] is
    true, omit conflicts that cannot result in inadequacy in the context of conflict resolution
    (i.e. conflicts that resolve to shift). The pseudo-end (⊥) symbol is omitted, because this
    function is used for attributing conflict contributions, and conflicting actions on ⊥ are a
    special case to which conflict attributions do not apply. *)

val conflicts: ?filter_pseudo_end:bool -> t -> uns
(** [conflicts ~filter_pseudo_end t] returns the number of conflicts in [t]. Pseudo-end (⊥)
    conflicts are omitted if [filter_pseudo_end] is true (default true). *)

val sr_conflicts: t -> uns
(** [sr_conflicts t] returns the number of shift-reduce conflicts in [t]. *)

val rr_conflicts: t -> uns
(** [rr_conflicts t] returns the number of reduce-reduce conflicts in [t]. *)
