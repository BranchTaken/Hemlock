(** Work queue used for managing 1) breadth-first lane tracing, 2) LR(1) item set closures which
    require (re)processing during {!module:Spec} state machine generation, 3) states which require
    (re)processing during {!module:Spec} split-stability closure. The work queue is typically
    appended to ([push_back]), but especially in the case of LR(1) item set closures, if the element
    currently being processed directly merges with itself, the result is instead prepended ([push])
    for immediate reprocessing. Elements which are already in the work queue maintain their position
    rather than being moved to the end of the queue. *)

open! Basis
open! Basis.Rudiments

type ('a, 'cmp) t

type ('a, 'cmp) cmper = (module Cmper.SMono with type t = 'a and type cmper_witness = 'cmp)

val pp: ('a, 'cmp) t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp t formatter] applies a formatted representation of [t] to the [formatter]. *)

val empty: ('a, 'cmp) cmper -> ('a, 'cmp) t
(** [empty cmper] returns an empty work queue. *)

val length: ('a, 'cmp) t -> uns
(** [length t] returns the number of elements in [t]. *)

val is_empty: ('a, 'cmp) t -> bool
(** [is_empty t] returns true iff the length of [t] is 0. *)

val push: 'a -> ('a, 'cmp) t -> ('a, 'cmp) t
(** [push a t] prepends [a]. [a] must not be present in [t] prior to [push]. *)

val push_back: 'a -> ('a, 'cmp) t -> ('a, 'cmp) t
(** [push a t] appends [a]. [a] must not be present in [t] prior to [push_back]. *)

val pop: ('a, 'cmp) t -> 'a * ('a, 'cmp) t
(** [pop t] removes the front element from [t] and returns the element along with the depleted [t].
*)

val mem: 'a -> ('a, 'cmp) t -> bool
(** [mem a t] returns true iff [a] is present in [t]. *)

val set: ('a, 'cmp) t -> ('a, 'cmp) Set.t
(** [set t] returns the set of elements in [t]. *)
