(** Automaton tracing, used for unreachable state removal (i.e. garbage collection). *)

open! Basis
open! Basis.Rudiments

type t

val init: Prods.t -> State.t array -> t
(** [init prods states] initializes an automaton tracer. *)

val reachable_state_indexes: Io.t -> t -> Io.t * (State.Index.t, State.Index.cmper_witness) Ordset.t
(** [reachable_state_indexes io t] traces the automaton in [t] from its roots (start states) and
    returns the set of reachable states. *)
