(** Characteristic finite state machine (CFSM) goto state nub, which lacks the LR(1) item set
    closure, actions and gotos of a state. *)

open! Basis
open! Basis.Rudiments

type t = {
  goto: Lr1Itemset.t;
  (** Goto state kernel. *)

  transit_contribs: TransitContribs.t;
  (** Transit conflict contributions. *)

  contribs: Contribs.t;
  (** Memoized contribs computed for `goto` in the context of `transit_contribs`. *)
}

include IdentifiableIntf.S with type t := t

val init: goto:Lr1Itemset.t -> transit_contribs:TransitContribs.t -> t
(** [init ~goto ~transit_contribs] initializes a goto nub with given [goto] kernel and conflict
    [transits_contribs]. *)

val core: t -> Lr0Itemset.t
(** [core t] returns the LR(0) item set corresponding to the goto kernel in [t], i.e. the goto
    kernel with no lookahead. *)
