open Seq_intf

module Make_def (T : I_mono_def) : S_mono_def with type t := T.t
                                               and type elm := T.elm
module Make_indef (T : I_mono_indef) : S_mono_indef with type t := T.t
                                                     and type elm := T.elm
