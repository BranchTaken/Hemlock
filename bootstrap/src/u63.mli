open Rudiments_uint

type t = uint
include Intnb_intf.S_u with type t := t

val to_int: t -> int
val of_int: int -> t
val kv: int -> t
