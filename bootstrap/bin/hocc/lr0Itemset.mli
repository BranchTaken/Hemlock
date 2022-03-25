(** Set of LR(0) items. Such itemsets do not commonly contain LR(0) items with differing positions
    within the same production, so naïve set implementation suffices. *)

open Basis
open! Basis.Rudiments

type t = (Lr0Item.t, Lr0Item.cmper_witness) Ordset.t

include IdentifiableIntf.S with type t := t

val equal: t -> t -> bool

val empty: t

val singleton: Lr0Item.t -> t

val mem: Lr0Item.t -> t -> bool

val insert: Lr0Item.t -> t -> t

val remove: Lr0Item.t -> t -> t

val union: t -> t -> t
