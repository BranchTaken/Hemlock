(** State antecedents lookup table, where each distinct (acyclic) path is a lane. The state graph
    only encodes forward transitions, but lane tracing typically works backwards from a given state.
*)

open! Basis
open! Basis.Rudiments

type t

val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

val length: t -> uns

val init: State.t array -> t

val antes_of_state_index: State.Index.t -> t -> State.Index.t array

val antes_of_state: State.t -> t -> State.Index.t array
