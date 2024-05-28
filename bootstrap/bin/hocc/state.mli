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

type t = {
  statenub: StateNub.t;
  (** State nub, which contains the LR(1) item set closure and inadequacy attributions. *)

  actions:
    (Symbol.Index.t, (Action.t, Action.cmper_witness) Ordset.t, Symbol.Index.cmper_witness)
      Ordmap.t;
  (** Per symbol action sets (i.e. potentially ambiguous). *)

  gotos: (Symbol.Index.t, Lr1ItemsetClosure.Index.t, Symbol.Index.cmper_witness) Ordmap.t;
  (** Per symbol gotos, which are consulted during reduction state transitions. *)
}

include IdentifiableIntf.S with type t := t

val init: resolve:bool -> Symbols.t -> Prods.t -> Isocores.t
  -> gotonub_of_statenub_goto:(StateNub.t -> Lr1Itemset.t -> GotoNub.t) -> StateNub.t -> t
(** [init ~resolve symbols prods isocores ~gotonub_of_statenub_goto statenub] creates a state based
    on [statenub]. *)

val remergeable: (Index.t, Index.t, Index.cmper_witness) Ordmap.t -> t -> t -> bool
(** [remergeable remergeable_state_map t0 t1] returns true if [t0] and [t1] have identical actions
    and gotos, thus making them remergeable despite any kernel differences. The keys
    [remergeable_state_map] are indices of states already known to be remergeable, and the values
    are the indices for the states they will be remerged with. *)

val remerge: Symbols.t -> (Index.t, Index.t, Index.cmper_witness) Ordmap.t-> t -> t -> t
(** [remerge symbols index_map t0 t1] creates a merged state comprising remergeable states
    [t0] and [t1]. *)

val reindex: (Index.t, Index.t, Index.cmper_witness) Ordmap.t -> t -> t
(** [reindex index_map t] creates a state with all LR(1) item set closure, state nub, and state
    indexes translated according to [index_map], where keys are the original indexes, and values are
    the reindexed indexes. *)

val index: t -> Index.t
(** [index t] returns the index of the contained unique LR(1) item set closure. *)

val is_start: t -> bool
(** [is_start t] returns true if [t] is a start state. *)

val has_pseudo_end_conflict: t -> bool
(** [has_pseudo_end_conflict t] returns true if the state conflicts on the pseudo-end (⊥) symbol. *)

val has_conflict_attribs: resolve:bool -> Symbols.t -> Prods.t -> t -> bool
(** [has_conflict_attribs ~resolve symbols prods t] returns true iff there are conflict
    attributions, i.e. per symbol conflict attributions. If [resolve] is true, omit conflicts that
    cannot result in inadequacy in the context of conflict resolution (i.e. conflicts that resolve
    to shift). The pseudo-end (⊥) symbol is omitted, because this function is used for attributing
    conflict attributions, and conflicting actions on ⊥ are a special case to which conflict
    attributions do not apply. *)

val conflict_attribs: resolve:bool -> Symbols.t -> Prods.t -> t -> Attribs.t
(** [conflict_attribs ~resolve symbols prods t] returns conflict attributions. If [resolve] is true,
    omit conflicts that cannot result in inadequacy in the context of conflict resolution (i.e.
    conflicts that resolve to shift). The pseudo-end (⊥) symbol is omitted, because this function is
    used for attributing conflict contributions, and conflicting actions on ⊥ are a special case to
    which conflict attributions do not apply. *)

val conflicts: ?filter_pseudo_end:bool -> t -> uns
(** [conflicts ~filter_pseudo_end t] returns the number of conflicts in [t]. Pseudo-end (⊥)
    conflicts are omittid if [filter_pseudo_end] is true (default true). *)

val sr_conflicts: t -> uns
(** [sr_conflicts t] returns the number of shift-reduce conflicts in [t]. *)

val rr_conflicts: t -> uns
(** [rr_conflicts t] returns the number of reduce-reduce conflicts in [t]. *)
