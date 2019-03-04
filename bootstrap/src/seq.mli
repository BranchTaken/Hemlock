open Seq_intf

module Make_def (T : I_def) : S_def with type t := T.t and type elm := T.elm
module Make_indef (T : I_indef) : S_indef with type t := T.t
                                           and type elm := T.elm
