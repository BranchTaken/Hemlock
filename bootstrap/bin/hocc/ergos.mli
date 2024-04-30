(** State successors lookup table, equivalent to {!module:Antes}, but with all edges reversed. *)

open! Basis
open! Basis.Rudiments

type t

val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp t] formats [t]. *)

val init: Antes.t -> t
(** [init antes] returns an ergos lookup table, i.e. a table with the source/destination states
    reversed relative to the entries in [antes]. *)

val ergos_of_state_index: State.Index.t -> t -> State.Index.t array
(** [ergos_of_state_index state_index t] returns an array of ergo state indices of the state
    corresponding to [state_index]. *)

val ergos_of_state: State.t -> t -> State.Index.t array
(** [ergos_of_state state t] returns an array of ergo state indices of [state]. *)
