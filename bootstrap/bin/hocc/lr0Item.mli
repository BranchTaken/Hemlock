(** LR(0) item, i.e. a dot-denoted position within a production and no lookahead. *)

open Basis
open Basis.Rudiments

type t = {
  prod: Prod.t;
  (** Production. *)

  dot: uns;
  (** Position within or at either end of [prod], where e.g. 1 means after element 0 and before
      element 1 (if any). *)
}

include IdentifiableIntf.S with type t := t

val pp_hr: Symbols.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs LR(0) item in human-readable form. *)

val init: prod:Prod.t -> dot:uns -> t
(** [init ~prod ~dot] creates an LR(0) item. *)
