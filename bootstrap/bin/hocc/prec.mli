(** Precedence and optional associativity. Precedences induce a directed acyclic, potentially
    disjoint, graph. The graph is processed to determine dominator relationships; not all
    precedences need be related. *)

open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  index: Index.t;
  (** Unique precedence index. *)

  name: string;
  (** Specified precedence name. *)

  assoc: Assoc.t option;
  (** Corresponding associativity, if any. *)

  doms: (Index.t, Index.cmper_witness) Ordset.t;
  (** Set of precedences which dominate this precedence. *)

  stmt: Parse.nonterm_prec;
  (** Declaration AST. *)
}

include FormattableIntf.SMono with type t := t

val pp_hr: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence in human-readable form. *)

val src_fmt: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence in hocc syntax. *)

val init: index:Index.t -> name:string -> assoc:(Assoc.t option)
  -> doms:(Index.t, Index.cmper_witness) Ordset.t -> stmt:Parse.nonterm_prec -> t
(** Used only by [Precs.init]. *)
