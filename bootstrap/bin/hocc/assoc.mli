(** Operator associativity. *)

open Basis

type t =
  | Left
  | Right

include IdentifiableIntf.S with type t := t
