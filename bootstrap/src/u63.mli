(* Partial Rudiments. *)
module Int = I63
type int = Int.t

type t = int
include Intnb_intf.S_u with type t := t

val to_int: t -> int
val of_int: int -> t
