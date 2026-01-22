(** Collection of remergeable state nub sets. While conceptually simple, the finicky maintenance
    details of a canonical index map warrants this separate module. *)

open! Basis
open! Basis.Rudiments

type t

val empty: t
(** [empty] returns an empty [t]. *)

val mem: StateNub.t -> t -> bool
(** [mem statenub t] returns true if [statenub] is in [t]. *)

val insert: StateNub.t -> StateNub.t -> t -> t
(** [insert statenub0 statenub1 t] inserts [statenub0] and [statenub1] as remergeable state nubs
    into a derivative of [t]. At most one of [statenub0] and [statenub1] can be a member of [t]
    prior to calling this function. *)

val index_map: t -> (StateNub.Index.t, StateNub.Index.t, StateNub.Index.cmper_witness) Ordmap.t
(** [index_map t] returns a map of remergeable statenub indexes in canonical form.

     Example: Given remergeable set {1, 2, 3}, the map contains [(2, 1); [3, 1)]. *)
