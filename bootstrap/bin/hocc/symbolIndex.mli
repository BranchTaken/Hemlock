(** Distinct from `Symbol.Index` to prevent mutual dependency between `Pred` and `Symbol`. *)

open Basis
open! Basis.Rudiments

include (module type of Uns)
