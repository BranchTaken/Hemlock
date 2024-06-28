(** Set of LR(0) items. Such itemsets do not commonly contain LR(0) items with differing positions
    within the same production, so naïve set implementation suffices. *)

open Basis
open! Basis.Rudiments

type t = (Lr0Item.t, Lr0Item.cmper_witness) Ordset.t

include IdentifiableIntf.S with type t := t

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns an empty LR(0) itemset. *)

val singleton: Lr0Item.t -> t
(** [singleton lr0item] returns an LR(0) itemset containing only [lr0item]. *)

val mem: Lr0Item.t -> t -> bool
(** [mem lr0item t] returns true if [t] contains [lr0item]. *)

val insert: Lr0Item.t -> t -> t
(** [insert lr0item t] returns a derivative of [t] containing [lr0item]. *)

val remove: Lr0Item.t -> t -> t
(** [remove lr0item t] returns a derivative of [t] not containing [lr0item]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of LR(0) itemsets in [t0] and [t1]. *)
