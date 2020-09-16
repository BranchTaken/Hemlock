(** 63-bit signed integer type. *)

open RudimentsInt0

type t = sint
include IntnbIntf.SI with type t := t

val of_int: int -> t
(** Initialize from an OCaml integer. *)

val kv: int -> t
(** Create a constant value. *)
