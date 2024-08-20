(* Symbol type. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val is_synthetic: t -> bool
(** [is_synthetic t] returns true if [t] is an synthetic symbol type, false otherwise. *)

val is_explicit: t -> bool
(** [is_explicit t] returns true if [t] is an explicit symbol type, false otherwise. *)

val to_string: t -> string
(** [to_string t] returns a syntactically valid string representation of [t], or an empty string if
    [t] is implicit. *)

val synthetic_implicit: t
(** [synthetic_implicit] returns synthetic implicit symbol type. *)

val implicit: t
(** [implicit] returns non-synthetic implicit symbol type. *)

val synthetic_explicit: string -> t
(** [synthetic_explicit type_] returns synthetic explicit type. *)

val explicit: string -> t
(** [explicit type_] returns non-synthetic explicit type. *)

val qualify: string -> t -> t
(** [qualify qualifier t] prefixes the type with a qualifier, e.g. [B.t] becomes [A.B.t]. *)

val synthetic_wrapper: t -> t
(** [synthetic_wrapper t] returns a synthetic wrapper of non-synthetic [t]. *)
