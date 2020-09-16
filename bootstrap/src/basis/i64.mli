(** 64-bit unsigned integer type. *)

open RudimentsInt0

type t = i64

include IntnbIntf.S with type t := t
include IntnbIntf.SSigned with type t := t
