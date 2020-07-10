(** 2-bit signed integer. *)

(* Partial Rudiments. *)
open Rudiments_int

type t
include Intnb_intf.S_i with type t := t

val to_int: t -> int
(** Convert to full-width signed integer. *)

val of_int: int -> t
(** Initialize from full-width signed integer, with possible loss. *)

val of_int_hlt: int -> t
(** Initialize from full-width signed integer, or halt if conversion would be
    lossy. *)

val kv: int -> t
(** Create constant value.  This is a stopgap solution for the lack of
    bitwidth-specific literals. *)

val to_uns: t -> uns
(** Convert to full-width unsigned integer. *)

val of_uns: uns -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_uns_hlt: uns -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)
