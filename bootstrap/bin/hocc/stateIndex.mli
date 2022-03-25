(** Distinct from `State.Index` to prevent mutual dependency between `Lr1ItemsetClosure` and
    `State`. *)

open! Basis
open! Basis.Rudiments

(* State indexes are isomorphic with `Lr1ItemsetClosure` indexes. *)
include (module type of Uns)
