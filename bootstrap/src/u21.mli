(* Partial Rudiments. *)
module Int = I63
module Uint = U63
type int = Int.t
type uint = Uint.t

type t = uint
include Intnb_intf.S_u with type t := t

val to_int: t -> int
val of_int: int -> t
val of_int_hlt: int -> t

val to_uint: t -> uint
val of_uint: uint -> t
val of_uint_hlt: uint -> t

val of_char: char -> t

val nul: t
val soh: t
val stx: t
val etx: t
val eot: t
val enq: t
val ack: t
val bel: t
val bs: t
val ht: t
val lf: t
val nl: t
val vt: t
val ff: t
val cr: t
val so: t
val si: t
val dle: t
val dc1: t
val dc2: t
val dc3: t
val dc4: t
val nak: t
val syn: t
val etb: t
val can: t
val em: t
val sub: t
val esc: t
val fs: t
val gs: t
val rs: t
val us: t
val del: t
