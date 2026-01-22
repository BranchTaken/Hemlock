(** Collection of state nubs that is organized into isocore sets, where each isocore set's LR(1)
    item sets are mutually incompatible despite having equal LR(0) cores.

    Once an isocores collection is fully populated, each state nub corresponds to a unique
    characteristic finite state machine (CFSM) state. By definition LALR(1) isocore sets are
    singletons, but more sophisticated algorithms may generate incompatible state nubs to prevent
    unnecessary grammar conflicts. *)

open! Basis
open! Basis.Rudiments

type t

val init: compat:(GotoNub.t -> StateNub.t -> bool) -> t
(** [init ~compat] creates an empty isocores collection for which LR(1) item set compatibility is
    determined by the [compat] function. *)

val mem: Lr0Itemset.t -> t -> bool
(** [mem core t] returns true if [t] contains an isocore set with the specified [core]. *)

val mems: Lr0Itemset.t -> t -> (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t
(** [mems core t] returns the isocore set corresponding to the specificed [core]. *)

val get: GotoNub.t -> t -> StateNub.Index.t option
(** [get gotonub t] returns the state nub in [t] that is compatible with [gotonub], or [None] if no
    such state nub exists. *)

val get_hlt: GotoNub.t -> t -> StateNub.Index.t
(** [get gotonub t] returns the state nub in [t] that is compatible with [gotonub], or halts if no
    such state nub exists. *)

val get_isocore_set_hlt: Lr0Itemset.t -> t
  -> (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t
(** [get_isocore_set_hlt core t] gets the set of state nub indices corresponding to the isocore set
    containing the state nubs with isocore equal to that of [core]. *)

val get_core_hlt: Lr0Itemset.t -> t -> StateNub.Index.t
(** [get_core_hlt core t] gets the index of the state nub with isocore equal to that of [core],
    under the assumption that [t] was fully generated, using the LALR(1) algorithm. *)

val insert: Symbols.t -> GotoNub.t -> t -> StateNub.Index.t * t
(** [insert symbols gotonub t] constructs a state nub which incorporates [gotonub], inserts it into
    an incremental derivative of [t], and returns its index along with the derivative of [t]. If the
    result establishes a new isocore set, the isocore set's sequence number is automatically
    assigned unless [GotoNub] carries an isocore set sequence number. *)

val merge: Symbols.t -> GotoNub.t -> StateNub.Index.t -> t -> bool * t
(** [merge symbols gotonub statenub_index t] merges [gotonub] into the state nub with given
    [statenub_index]. If the resulting state nub is distinct from the input, true is returned along
    with a derivative of [t] containing the resulting state nub; [false, t] otherwise. *)

val remove_hlt: StateNub.Index.t -> t -> t
(** [remove_hlt statenub_index t] removes the state nub with given [statenub_index]. *)

val remerge: Symbols.t
  -> (StateNub.Index.t, StateNub.Index.t, StateNub.Index.cmper_witness) Ordmap.t -> StateNub.Index.t
  -> StateNub.Index.t -> t -> t
(** [remerge symbols index_map statenub_index0 statenub_index1 t] creates a merged state nub
    comprising the remergeable state nubs corresponding to [statenub_index0] and [statenub_index1]
    and replaces the lower-indexed state nub with the merged result in a derivative of [t]. *)

val reindex: StateIndexMap.t -> t -> t
(** [reindex state_index_map t] creates a derivative of [t] with all LR(1) item set closure and
    state nub indexes translated according to [state_index_map]. State nubs without mappings are
    omitted from the result. *)

val isocores_length: t -> uns
(** [isocores_length t] returns the number of isocore sets in [t]. *)

val length: t -> uns
(** [length t] returns the number of state nubs in [t] (greater than or equal to [isocores_length
    t]). *)

val statenub: StateNub.Index.t -> t -> StateNub.t
(** [statenub statenub_index t] returns the state nub in [t] with given [statenub_index]. *)

val fold: init:'accum -> f:('accum -> StateNub.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the state nubs in [t], in increasing state nub
    index order. *)

val fold_isocore_sets: init:'accum
  -> f:('accum -> (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t -> 'accum) -> t
  -> 'accum
(** [fold_isocore_sets ~init ~f t] iteratively applies [f] to the isocore sets in [t]. *)
