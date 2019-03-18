open Rudiments_uint
open Intnb_intf

module Make_i (T : I) : S_i with type t := int
module Make_u (T : I) : S_u with type t := uint
