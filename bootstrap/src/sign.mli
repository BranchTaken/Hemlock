type t =
| Neg
| Zero
| Pos

include Stringable_intf.S with type t := t

val of_int: int -> t
val to_int: t -> int
val to_float: t -> float

val flip: t -> t
val ( * ): t -> t -> t
