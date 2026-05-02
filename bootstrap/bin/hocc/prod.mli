(** Production. *)

open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  index: Index.t;
  (** Unique production index. *)

  lhs_index: SymbolIndex.t;
  (** LHS symbol index. *)

  rhs_indexes: SymbolIndex.t array;
  (** RHS symbol indexes in left-to-right order. *)

  prec: Prec.t option;
  (** Precedence, if any. This is denormalized with respect to the hocc specification, such that it
      is [Some p] regardless of whether precedence is specified for just this prod versus all of the
      nonterm (LHS symbol) prods. *)

  stmt: Parse.nonterm_prod option;
  (** Declaration AST. *)

  callback: Callback.t;
  (** Reduction callback code. *)
}

include IdentifiableIntf.S with type t := t

val init: index:Index.t -> lhs_index:SymbolIndex.t -> rhs_indexes:SymbolIndex.t array
  -> prec:Prec.t option -> stmt:Parse.nonterm_prod option -> callback:Callback.t -> t
(** Used only by [Prods.init]. *)

val is_synthetic: t -> bool
(** [is_synthetic t] returns true iff [t] is a synthetic production, i.e. it has no explicit
    representation in the hocc specification. *)

val is_epsilon: t -> bool
(** [is_epsilon t] returns true if [t] is an ε production, i.e. it has an empty RHS. *)
