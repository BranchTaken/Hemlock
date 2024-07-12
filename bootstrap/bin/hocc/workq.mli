(** Work queue used for managing 1) LR(1) item set closures which require (re)processing during
    {!module:Spec} state machine generation, and 2) states which require (re)processing during
    {!module:Spec} split-stability closure. The work queue is typically appended to ([push_back]),
    but in the case of LR(1) item set closures, if the closure currently being processed directly
    merges with itself, the result is instead prepended ([push]) for immediate reprocessing.
    Elements which are already in the work queue maintain their position rather than being moved to
    the end of the queue. *)

open! Basis
open! Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val empty: t
(** [empty] returns an empty work queue. *)

val length: t -> uns
(** [length t] returns the number of elements in [t]. *)

val is_empty: t -> bool
(** [is_empty t] returns true iff the length of [t] is 0. *)

val push: Lr1ItemsetClosure.Index.t -> t -> t
(** [push i t] prepends index [i]. [i] must not be present in [t] prior to [push]. *)

val push_back:  Lr1ItemsetClosure.Index.t -> t -> t
(** [push i t] appends index [i]. [i] must not be present in [t] prior to [push_back]. *)

val pop: t -> Lr1ItemsetClosure.Index.t * t
(** [pop t] removes the front index from [t] and returns the index along with the depleted [t]. *)

val mem: Lr1ItemsetClosure.Index.t -> t -> bool
(** [mem i t] returns true iff [i] is present in [t]. *)

val set: t -> (Lr1ItemsetClosure.Index.t, Lr1ItemsetClosure.Index.cmper_witness) Set.t
(** [set t] returns the set of indices in [t]. *)
