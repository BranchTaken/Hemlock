(** 64-bit unsigned integer type. *)

open Rudiments_int0

type t = i64

include Intnb_intf.S with type t := t
include Intnb_intf.S_signed with type t := t
