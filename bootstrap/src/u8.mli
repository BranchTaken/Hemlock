(** 8-bit unsigned integer. *)

(* Partial Rudiments. *)
open Rudiments_uint
module Codepoint = U21
type codepoint = Codepoint.t

type t
include Intnb_intf.S_u with type t := t

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

val to_uint: t -> uint
(** Convert to full-width unsigned integer. *)

val of_uint: uint -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_uint_hlt: uint -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)

val to_codepoint: t -> codepoint
(** Convert to codepoint. *)

val of_codepoint: codepoint -> t
(** Initialize from codepoint, with possible loss. *)

val of_codepoint_hlt: codepoint -> t
(** Initialize from codepoint, or halt if conversion would be lossy. *)
