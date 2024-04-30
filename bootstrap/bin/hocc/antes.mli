(** State antecedents lookup table, where each distinct (acyclic) path is a lane. The state graph
    only encodes forward transitions, but lane tracing typically works backwards from a given state.
*)

open! Basis
open! Basis.Rudiments

type t

val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp t] formats [t]. *)

val length: t -> uns
(** [length t] returns the number of antecedent entries in [t]. *)

val init: State.t array -> t
(** [init states] returns an antecedents lookup table with one entry for each state transition
    encoded in [states]. *)

val antes_of_state_index: State.Index.t -> t -> State.Index.t array
(** [antes_of_state_index state_index t] returns an array of antecedent state indices of
    the state corresponding to [state_index]. *)

val antes_of_state: State.t -> t -> State.Index.t array
(** [antes_of_state state t] returns an array of antecedent state indices of [state]. *)
