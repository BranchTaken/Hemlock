(* 512-bit unsigned integer.

    See {!module:ConvertIntf} for documentation on conversion functions. *)

type t
include IntwIntf.SFU with type t := t

val bits_of_i512: I512.t -> t
val bits_to_i512: t -> I512.t
val like_of_i512_opt: I512.t -> t option
val like_to_i512_opt: t -> I512.t option
val like_of_i512_hlt: I512.t -> t
val like_to_i512_hlt: t -> I512.t
