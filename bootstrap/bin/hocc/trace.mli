(** Automaton tracing garbage collection, used for unreachable state removal. *)

open! Basis
open! Basis.Rudiments

val gc_states: Io.t -> Prods.t -> Isocores.t -> State.t array -> Io.t * Isocores.t * State.t array
(** [gc_states io prods isocores states] traces the automaton encoded by [states] to identify
    reachable actions, gotos, and states, then creates a compacted automaton comprising only
    reachable transitions and states. *)
