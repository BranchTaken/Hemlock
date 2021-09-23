(** 64-bit unsigned integer type. *)

open RudimentsInt0

type t = uns

include IntnbIntf.S with type t := t

val to_i64: t -> sint
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_i64: sint -> t
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val to_sint: t -> sint
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_sint: sint -> t
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val to_int: t -> int
(** Convert to default-width integer, with possible loss. *)

val to_int_opt: t -> int option
(** Convert to default-width integer, or return [None] if conversion would be lossy. *)

val to_int_hlt: t -> int
(** Convert to default-width integer, or halt if conversion would be lossy. *)

val of_int: int -> t
(** Initialize from a default-width integer. *)
