(** Boolean type. *)

open Rudiments0

type t = bool

include IdentifiableIntf.S with type t := t
include StringableIntf.S with type t := t

val of_uns: uns -> t
(** Initialize from unsigned integer. *)

val to_uns: t -> uns
(** Convert to unsigned integer. *)

val not: t -> t
(** [not t] returns the logical negation of [t]. *)

(* The && and || operators built in to OCaml are magically short-circuiting. If
 * we define equivalent operators they lose the short-circuiting magic. *)
(*
val ( && ): t -> t -> t
val ( || ): t -> t -> t
*)
