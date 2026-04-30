(** Precedence. *)

open Basis
open! Basis.Rudiments

module Index = PrecIndex
type t = {
  name_index: Index.t;
  (** Index of precedence name within containing precedence set. *)

  prec_set_index: PrecSet.Index.t;
  (** Index of containing precedence set. *)
}

include FormattableIntf.SMono with type t := t

val init: name:string -> prec_set:PrecSet.t -> t
