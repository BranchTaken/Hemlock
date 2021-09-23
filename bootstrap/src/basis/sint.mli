(** 64-bit signed integer type. *)

open RudimentsInt0

type t = sint

include IntnbIntf.S with type t := t
include IntnbIntf.SSigned with type t := t

val kv: int64 -> t
(** Create a constant value. *)

val to_int: t -> int
(** Convert to a default-width integer, with possible loss. *)

val to_int_opt: t -> int option
(** Convert to a default-width integer, or return [None] if conversion would be lossy. *)

val to_int_hlt: t -> int
(** Convert to a default-width integer, or halt if conversion would be lossy. *)

val of_int: int -> t
(** Initialize from a default-width integer. *)
