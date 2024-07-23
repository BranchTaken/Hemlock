(** Qualified symbol type. *)

open Basis
open! Basis.Rudiments

type explicit = {
  module_: string;
  type_: string;
} (** Explicit qualified type. *)

type t = {
  synthetic: bool; (** Synthetic symbol if true. *)
  explicit_opt: explicit option; (** Some explicit qualified type, or None (unspecified type, e.g.
                                     simple [token SOME_TOKEN] or reductionless production). *)
}

include IdentifiableIntf.S with type t := t

val synthetic_implicit: t
(** [synthetic_implicit] returns synthetic implicit qualified type. *)

val implicit: t
(** [implicit] returns non-synthetic implicit qualified type. *)

val synthetic_explicit: module_:string -> type_:string -> t
(** [synthetic_explicit ~module_ ~type_] returns synthetic explicit qualified type. *)

val explicit: module_:string -> type_:string -> t
(** [explicit ~module_ ~type_] returns non-synthetic explicit qualified type. *)

val synthetic_wrapper: t -> t
(** [synthetic_wrapper t] returns a synthetic wrapper of non-synthetic [t]. *)
