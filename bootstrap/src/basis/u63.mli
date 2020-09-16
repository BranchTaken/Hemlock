(** 63-bit unsigned integer type. *)

open RudimentsInt0

type t = uns
include IntnbIntf.SU with type t := t

val to_sint: t -> sint
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_sint: sint -> t
(** Convert a signed integer to a bitwise identical unsigned integer. *)
