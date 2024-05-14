(** Tri-state type. *)

open! Basis
open! Basis.Rudiments

type t =
  | No
  | Maybe
  | Yes

include IdentifiableIntf.S with type t := t
include StringableIntf.S with type t := t
