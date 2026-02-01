(** Precedence set and optional associativity. Precedences induce a directed acyclic, potentially
    disjoint, graph. The graph is processed to determine dominator relationships; not all
    precedences need be related. *)

open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  index: Index.t;
  (** Unique precedence index. *)

  names: string array;
  (** Specified precedence names. *)

  assoc: Assoc.t option;
  (** Corresponding associativity, if any. *)

  doms: Bitset.t;
  (** Set of precedence sets which dominate this precedence. *)

  stmt: Parse.nonterm_prec_set;
  (** Declaration AST. *)
}

include FormattableIntf.SMono with type t := t

val pp_hr: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence set in human-readable form. *)

val src_fmt: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence set in hocc syntax. *)

val init: index:Index.t -> names:string array -> assoc:(Assoc.t option) -> doms:Bitset.t
  -> stmt:Parse.nonterm_prec_set -> t
(** Used only by [Precs.init]. *)
