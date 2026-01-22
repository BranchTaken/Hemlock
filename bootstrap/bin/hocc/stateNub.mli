(** Characteristic finite state machine (CFSM) state nub, which lacks the actions and gotos of a
    state. *)

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

  val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** Formatter which outputs action in human-readable form. *)
end

module Actionset: sig
  type t = (Action.t, Action.cmper_witness) Ordset.t

  val resolve: Symbols.t -> Prods.t -> Symbol.Index.t -> t -> t
  (** [resolve symbols prods symbol_index t] attempts to resolve conflicts, if any. Unresolvable
      conflicts are left intact. *)
end

type t = {
  lr1itemsetclosure: Lr1ItemsetClosure.t;
  (** LR(1) item set closure. *)

  isocores_sn: uns;
  (** Isocore set serial number for the set containing this state nub. *)

  isocore_set_sn: uns;
  (** Serial number for this state nub within its containing isocore set. *)

  kernel_attribs: KernelAttribs.t;
  (** Union of merged in-transit conflict attributions. *)

  attribs: Attribs.t;
  (** Memoized merged attribs. *)
}

include IdentifiableIntf.S with type t := t

val init: Symbols.t -> index:Index.t -> isocores_sn:uns -> isocore_set_sn:uns -> GotoNub.t -> t
(** [init symbols ~index ~isocores_sn ~isocore_set_sn gotonub] initializes a state nub with given
    [index], [isocores_sn], [isocore_set_sn], LR(1) item set closure based on the kernel of
    [gotonub], and conflict attributions of [gotonub]. *)

val index: t -> Index.t
(** [index t] returns the index of the contained unique LR(1) item set closure. *)

val isocores_sn: t -> uns
(** [isocores_sn t] returns the isocore set serial number for the set containing [t]. *)

val isocore_set_sn: t -> uns
(** [isocore_set_sn t] returns the serial number of [t] with respect to its containing isocore set.
*)

val remerge: Symbols.t -> (Index.t, Index.t, Index.cmper_witness) Ordmap.t -> t -> t -> t
(** [remerge symbols remergeable_index_map t0 t1] creates a merged state nub comprising remergeable
    state nubs [t0] and [t1]. *)

val reindex: StateIndexMap.t -> t -> t
(** [reindex state_index_map t] creates a state nub with all LR(1) item set closure indexes, state
    nub indexes, and isocore set sequence numbers translated according to [state_index_map]. *)

val merge: Symbols.t -> GotoNub.t -> t -> bool * t
(** [merge symbols gotonub t] merges the kernel represented by [gotonub] into [t]'s kernel and
    creates the closure of the merged kernel, as well as merging conflict attributions from
    [gotonub]. The boolean result indicates whether items were merged into the kernel. *)

val next: t -> (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t
(** [next t] returns the set of symbol indexes that may appear next, i.e. the symbol indexes
    corresponding to the symbols for which [goto] returns a non-empty set. *)

val goto: Symbol.t -> t -> Lr1Itemset.t
(** [goto symbol t] computes the kernel of the goto set reachable from [t], given [symbol]. *)

val actions: Symbols.t -> t -> (Symbol.Index.t, Actionset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [actions symbols t] computes the map of per symbol actions for [t]. *)

val gotos: Symbols.t -> t -> (Symbol.Index.t, Lr1Itemset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [gotos symbols t] computes the map of per non-terminal symbol gotos for [t]. *)

val filtered_kernel_attribs: t -> KernelAttribs.t
(** [filtered_kernel_attribs t] returns the kernel attribs in [t], filtered to contain only attribs
    which are relevant to the kernel follow sets. *)

val resolve: Symbols.t -> Prods.t
  -> (Symbol.Index.t, Actionset.t, Symbol.Index.cmper_witness) Ordmap.t
  -> (Symbol.Index.t, Actionset.t, Symbol.Index.cmper_witness) Ordmap.t
(** [resolve ~symbols ~prods actions] resolves conflicts in [actions] to the maximum degree possible
    given precedences. *)

val compat_lr1: GotoNub.t -> t -> bool
(** [compat_lr1 gotonub t] determines whether [gotonub] and the kernel of [t] are identical, which
    is the basis of the canonical LR(1) algorithm. *)

val compat_ielr1: resolve:bool -> Symbols.t -> Prods.t -> GotoNub.t -> t -> bool
(** [compat_ielr1 ~resolve symbols prods gotonub t] determines whether [gotonub] and [t] are
    split-stable (i.e. irrelevant to compatibility testing) and make compatible conflict
    attributions (if any) in the context of each {state,symbol} conflict. If [resolve] is true,
    conflicts which will be successfully resolved during state generation are treated as compatible
    to avoid pointless state duplication. This function is the basis of the IELR(1) algorithm. *)

val compat_pgm1: GotoNub.t -> t -> bool
(** [compat_pgm1 gotonub t] determines whether [gotonub] and [t] are weakly compatible, as defined
    by the Pager(1977) algorithm, and as refined by Menhir to prevent phantom conflicts accompanying
    actual conflicts. This function is the basis of the PGM(1) algorithm. *)

val compat_lalr1: GotoNub.t -> t -> bool
(** [compat_lalr1 gotonub t] determines whether [gotonub] has the same LR(0) kernel as that of the
    LR(1) kernel of [t], which is the basis of the LALR(1) algorithm. *)
