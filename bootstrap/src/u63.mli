type t = I63.t
include Intnb_intf.S_u with type t := t

val of_int: I63.t -> t
val to_int: t -> I63.t
