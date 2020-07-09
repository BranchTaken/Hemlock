(** 2-bit signed integer. *)

(* Partial Rudiments. *)
open Rudiments_int

type t
include Intnb_intf.S_i with type t := t

val to_isize: t -> isize
(** Convert to full-width signed integer. *)

val of_isize: isize -> t
(** Initialize from full-width signed integer, with possible loss. *)

val of_isize_hlt: isize -> t
(** Initialize from full-width signed integer, or halt if conversion would be
    lossy. *)

val kv: int -> t
(** Create constant value. This is a stopgap solution for the lack of
    bitwidth-specific literals. *)

val to_usize: t -> usize
(** Convert to full-width unsigned integer. *)

val of_usize: usize -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_usize_hlt: usize -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)
