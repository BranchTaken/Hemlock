(** 63-bit signed integer type. *)

open Rudiments_int0

type t = sint
include Intnb_intf.S_i with type t := t

val of_int: int -> t
(** Initialize from an OCaml integer. *)

val kv: int -> t
(** Create a constant value. *)
