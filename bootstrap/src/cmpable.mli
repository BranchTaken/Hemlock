open Cmpable_intf

module Make (T : I) : S with type t := T.t
module Make_zero (T : I_zero) : S_zero with type t := T.t
module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t
                                        and type 'a elm := 'a T.elm
