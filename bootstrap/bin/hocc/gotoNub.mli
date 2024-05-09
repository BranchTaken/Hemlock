(** Characteristic finite state machine (CFSM) goto state nub, which lacks the LR(1) item set
    closure, actions and gotos of a state. *)

open! Basis
open! Basis.Rudiments

type t = {
  goto: Lr1Itemset.t;
  (** Goto state kernel. *)

  transit_attribs: TransitAttribs.t;
  (** Transit conflict attributions. *)

  attribs: Attribs.t;
  (** Memoized attribs computed for [goto] in the context of [transit_attribs]. *)
}

include IdentifiableIntf.S with type t := t

val init: goto:Lr1Itemset.t -> transit_attribs:TransitAttribs.t -> t
(** [init ~goto ~transit_attribs] initializes a goto nub with given [goto] kernel and conflict
    [transit_attribs]. *)

val core: t -> Lr0Itemset.t
(** [core t] returns the LR(0) item set corresponding to the goto kernel in [t], i.e. the goto
    kernel with no lookahead. *)
