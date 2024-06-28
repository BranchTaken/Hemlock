(** LR(1) item, i.e. a dot-denoted position within a production and one symbol of lookahead. *)

open Basis
open! Basis.Rudiments

type t = {
  lr0item: Lr0Item.t;
  (** LR(0) item, i.e. a production with dot-denoted position. *)

  follow: (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t;
  (** Set of symbols which may immediately follow (aka lookahead). *)
}

include IdentifiableIntf.S with type t := t

val pp_hr: Symbols.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs LR(1) item in human-readable form. *)

val init: lr0item:Lr0Item.t -> follow:(Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t -> t
(** [init ~lr0item ~follow] creates an LR(1) item. *)

val first: Symbols.t -> t -> (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t
(** [first symbols t] computes the first set of [t]. The first set is not memoized because it is
    only needed during closure computation in [Lr1ItemsetClosure] (the [init] and [merge]
    functions), whereas many items may be created as goto set elements, but only compatible goto
    sets are merged. *)

val is_kernel_item: t -> bool
(** [is_kernel_item t] returns true iff [t] would be a valid kernel item. Kernel items must have
    non-zero production dot positions unless they reduce to synthetic start symbol wrappers. *)

val is_accept: t -> bool
(** [is_accept t] returns true iff [t] is compatible with an accept state, i.e. it is a kernel item
    with maximal dot position and follow set containing only the ⊥ symbol. *)

val follow_union: (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t -> t -> t
(** [follow_union symbol_indexes t] creates an LR(1) item equivalent to [t] with [symbol_indexes]
    merged into the follow set. *)
