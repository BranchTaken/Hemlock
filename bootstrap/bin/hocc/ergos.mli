(** State successors lookup table, equivalent to {!module:Antes}, but with all edges reversed. *)

open! Basis
open! Basis.Rudiments

type t

val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

val init: Antes.t -> t

val ergos_of_state_index: State.Index.t -> t -> State.Index.t array

val ergos_of_state: State.t -> t -> State.Index.t array
