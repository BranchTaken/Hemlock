(** 32-bit signed integer. *)

(* Partial Rudiments. *)
open RudimentsInt

type t
include IntnbIntf.SI with type t := t

val to_sint: t -> sint
(** Convert to full-width signed integer. *)

val of_sint: sint -> t
(** Initialize from full-width signed integer, with possible loss. *)

val of_sint_opt: sint -> t option
(** Initialize from full-width signed integer, or return [None] if conversion would be lossy. *)

val of_sint_hlt: sint -> t
(** Initialize from full-width signed integer, or halt if conversion would be lossy. *)

val kv: int64 -> t
(** Create constant value. This is a stopgap solution for the lack of bitwidth-specific literals. *)

val to_uns: t -> uns
(** Convert to full-width unsigned integer. *)

val of_uns: uns -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_uns_opt: uns -> t option
(** Initialize from full-width unsigned integer, or return [None] if conversion would be lossy. *)

val of_uns_hlt: uns -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be lossy. *)
