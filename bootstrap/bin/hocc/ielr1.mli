(** IELR(1)-specific functionality. *)

open! Basis
open! Basis.Rudiments

(** [gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states] generates a
    function, [gotonub_of_statenub_goto statenub goto], which is used during parser state
    generation. {!type:GotoNub.t} and {!type:StateNub.t} carry cumulative conflict contribution data
    associated with state transitions, which informs state compatibility testing as implemented by
    [StateNub.compat_ielr1] and [Contrib.compat_ielr1]. *)
val gen_gotonub_of_statenub_goto: resolve:bool -> Io.t -> Symbols.t -> Prods.t -> Isocores.t
  -> State.t array -> Io.t * (StateNub.t -> Lr1Itemset.t -> GotoNub.t)
