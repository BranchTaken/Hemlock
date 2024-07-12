(** State adjacency lookup table for transitions in the state graph, where each distinct (acyclic)
    path is a lane. The state graph only encodes forward transitions, but lane tracing typically
    works backwards from a given conflict state. *)

open! Basis
open! Basis.Rudiments

type t

val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp t] formats [t]. *)

val length: t -> uns
(** [length t] returns the number of transitions in [t]. *)

val init: State.t array -> t
(** [init states] returns a bidirectional adjacency lookup table with one logical entry for each
    state transition encoded in [states]. *)

val ipreds_of_state_index: State.Index.t -> t -> State.Index.t array
(** [ipreds_of_state_index state_index t] returns an array of immediate predecessor state indices of
    the state corresponding to [state_index]. *)

val ipreds_of_state: State.t -> t -> State.Index.t array
(** [ipreds_of_state state t] returns an array of immediate predecessor state indices of [state]. *)

val isuccs_of_state_index: State.Index.t -> t -> State.Index.t array
(** [isuccs_of_state_index state_index t] returns an array of immediat successor state indices of
    the state corresponding to [state_index]. *)

val isuccs_of_state: State.t -> t -> State.Index.t array
(** [isuccs_of_state state t] returns an array of immediate successor state indices of [state]. *)
