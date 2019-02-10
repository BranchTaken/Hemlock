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
