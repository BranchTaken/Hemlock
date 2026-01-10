(** Operator associativity. *)

open Basis

type t =
  | Left
  | Right
  | Nonassoc

include IdentifiableIntf.S with type t := t
