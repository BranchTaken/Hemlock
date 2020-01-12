open Cmpable_intf

module Make (T : I_mono) : S_mono with type t := T.t
module Make_zero (T : I_mono_zero) : S_mono_zero with type t := T.t
module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t
