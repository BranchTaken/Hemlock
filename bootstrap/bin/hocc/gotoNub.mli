(** Characteristic finite state machine (CFSM) goto state nub, which lacks the LR(1) item set
    closure, actions and gotos of a state. *)

open! Basis
open! Basis.Rudiments

type t = {
  goto: Lr1Itemset.t;
  (** Goto state kernel. *)

  isocores_sn_opt: uns option;
  (** Isocore set serial number for the set containing this goto nub. *)

  kernel_attribs: KernelAttribs.t;
  (** Transit conflict attributions. *)

  attribs: Attribs.t;
  (** Memoized attribs computed for [goto] in the context of [kernel_attribs]. *)
}

include IdentifiableIntf.S with type t := t

val init: isocores_sn_opt:uns option -> goto:Lr1Itemset.t -> kernel_attribs:KernelAttribs.t -> t
(** [init ~isocores_sn_opt ~goto ~kernel_attribs] initializes a goto nub with given
    [isocores_sn_opt], [goto] kernel, and conflict [kernel_attribs]. *)

val core: t -> Lr0Itemset.t
(** [core t] returns the LR(0) item set corresponding to the goto kernel in [t], i.e. the goto
    kernel with no lookahead. *)
