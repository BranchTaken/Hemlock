(** Automaton tracing, used for unreachable state removal (i.e. garbage collection). *)

open! Basis
open! Basis.Rudiments

type t

(** Shift can lead to all actions, whereas each reduce-goto can lead to precisely the one action
    corresponding to the lookahead symbol. This type records `Shift` if a shift ipred edge is
    present, the union of follow sets from reduce-goto ipred edges otherwise. *)
type reach =
  | Shift
  | Follows of (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t

val init: Prods.t -> State.t array -> t
(** [init prods states] initializes an automaton tracer. *)

val reachable: Io.t -> t -> Io.t * (State.Index.t, reach, State.Index.cmper_witness) Ordmap.t
(** [reachable io t] traces the automaton in [t] from its roots (start states) and returns the set
    of reachable states and their corresponding reachable actions. *)
