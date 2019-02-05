open Cmpable_intf

module Make_eq (T : I) : S_eq with type t := T.t
module Make_rel (T : I) : S_rel with type t := T.t
module Make_range (T : I) : S_range with type t := T.t
module Make_zero (T : I_zero) : S_zero with type t := T.t
