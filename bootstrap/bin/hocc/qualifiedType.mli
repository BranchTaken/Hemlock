(** Qualified symbol type. *)

open Basis
open! Basis.Rudiments

type t =
  | Synthetic (** Synthetic symbol. *)
  | Implicit (** Unspecified type, e.g. simple [token SOME_TOKEN] or reductionless production. *)
  | Explicit of {
      module_: string;
      type_: string;
    } (** Symbol with explicitly specified type.  *)

include IdentifiableIntf.S with type t := t

val synthetic: t
(** [synthetic] returns [Synthetic]. *)

val implicit: t
(** [implicit] returns [Implicit]. *)

val init: module_:string -> type_:string -> t
(** [init ~module_ ~type_] returns [Explicit {module_; type_}]. *)
