(** Characteristic finite state machine (CFSM) state nub, which lacks the actions and gotos of a
    state. *)

open Basis
open! Basis.Rudiments

(* Isomorphic with `State.Index`. *)
module Index = Uns

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
(** [remerge symbols remergeable_index_map t0 t1] re-merges state nub [t0] into [t1], where [t0] has
    a higher index than [t1]. *)

val reindex: StateIndexMap.t -> t -> t
(** [reindex state_index_map t] creates a state nub with all LR(1) item set closure indexes, state
    nub indexes, and isocore set sequence numbers translated according to [state_index_map]. *)

val merge: Symbols.t -> GotoNub.t -> t -> bool * t
(** [merge symbols gotonub t] merges the kernel represented by [gotonub] into [t]'s kernel and
    creates the closure of the merged kernel, as well as merging conflict attributions from
    [gotonub]. The boolean result indicates whether items were merged into the kernel. *)

val filtered_kernel_attribs: t -> KernelAttribs.t
(** [filtered_kernel_attribs t] returns the kernel attribs in [t], filtered to contain only attribs
    which are relevant to the kernel follow sets. *)

val compat_ielr: resolve:bool -> Symbols.t -> Prods.t -> GotoNub.t -> t -> bool
(** [compat_ielr ~resolve symbols prods gotonub t] determines whether [gotonub] and [t] are
    split-stable (i.e. irrelevant to compatibility testing) and make compatible conflict
    attributions (if any) in the context of each {state,symbol} conflict. If [resolve] is true,
    conflicts which will be successfully resolved during state generation are treated as compatible
    to avoid pointless state duplication. This function is the basis of the IELR(1) algorithm. *)

val compat_lr: GotoNub.t -> t -> bool
(** [compat_lr gotonub t] determines whether [gotonub] and the kernel of [t] are identical, which is
    the basis of the canonical LR(1) algorithm. *)

val compat_pgm: GotoNub.t -> t -> bool
(** [compat_pgm gotonub t] determines whether [gotonub] and [t] are weakly compatible, as defined by
    the Pager(1977) algorithm, and as refined by Menhir to prevent phantom conflicts accompanying
    actual conflicts. This function is the basis of the PGM LR(1) algorithm. *)

val compat_lalr: GotoNub.t -> t -> bool
(** [compat_lalr gotonub t] determines whether [gotonub] has the same LR(0) kernel as that of the
    LR(1) kernel of [t], which is the basis of the LALR(1) algorithm. *)
