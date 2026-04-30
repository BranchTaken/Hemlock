(** Distinct from `Prec.Index` to prevent mutual dependency between `PrecSet` and `Prec`. *)

open! Basis
open! Basis.Rudiments

(* Prec indexes are isomorphic with `PrecSet` indexes. *)
include (module type of Uns)
