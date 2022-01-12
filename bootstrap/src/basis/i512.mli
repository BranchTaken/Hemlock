(* 512-bit signed integer.

   See {!module:ConvertIntf} for documentation on conversion functions. *)

type t
include IntwIntf.SFI with type t := t

val trunc_of_zint: Zint.t -> t
val extend_to_zint: t -> Zint.t
val narrow_of_zint_opt: Zint.t -> t option
val narrow_of_zint_hlt: Zint.t -> t

val trunc_of_nat: Nat.t -> t
val narrow_of_nat_opt: Nat.t -> t option
val widen_to_nat_opt: t -> Nat.t option
val narrow_of_nat_hlt: Nat.t -> t
val widen_to_nat_hlt: t -> Nat.t
