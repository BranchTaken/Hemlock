(** 8-bit unsigned integer. *)

(* Partial Rudiments. *)
open Rudiments_int
module Codepoint = U21
type codepoint = Codepoint.t

type t
include Intnb_intf.S_u with type t := t

val to_isize: t -> isize
(** Convert to full-width signed integer. *)

val of_isize: isize -> t
(** Initialize from full-width signed integer, with possible loss. *)

val of_isize_hlt: isize -> t
(** Initialize from full-width signed integer, or halt if conversion would be
    lossy. *)

val kv: int -> t
(** Create constant value.  This is a stopgap solution for the lack of
    bitwidth-specific literals. *)

val to_usize: t -> usize
(** Convert to full-width unsigned integer. *)

val of_usize: usize -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_usize_hlt: usize -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)

val of_char: char -> t
(** Initialize from character literal.  This is a stopgap for the lack of
    codepoint literals. *)

val to_codepoint: t -> codepoint
(** Convert to codepoint. *)

val of_codepoint: codepoint -> t
(** Initialize from codepoint, with possible loss. *)

val of_codepoint_hlt: codepoint -> t
(** Initialize from codepoint, or halt if conversion would be lossy. *)
