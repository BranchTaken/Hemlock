(** 64-bit signed integer type. *)

open RudimentsInt0

type t = i64

include IntnbIntf.S with type t := t
include IntnbIntf.SSigned with type t := t

val to_sint: t -> sint
(** Convert to a default-width signed integer, with possible loss. *)

val to_sint_opt: t -> sint option
(** Convert to a default-width signed integer, or return [None] if conversion
    would be lossy. *)

val to_sint_hlt: t -> sint
(** Convert to a default-width signed integer, or halt if conversion would be
    lossy. *)

val of_sint: sint -> t
(** Initialize from a default-width signed integer. *)
