(** 63-bit signed integer type. *)

type t = int
include Intnb_intf.S_i with type t := t
