(** Precedence. *)

open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  name_index: Index.t;
  (** Index of precedence name within containing precedence set. *)

  prec_set: PrecSet.t;
  (** Containing precedence set. *)
}

include FormattableIntf.SMono with type t := t

val name: t -> string
(** [name t] returns the precedence name of [t]. *)

val pp_hr: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence in human-readable form. *)

val src_fmt: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs precedence in hocc syntax. *)

val init: name:string -> prec_set:PrecSet.t -> t
