(** IELR(1)-specific functionality. Although this module drives IELR(1) automaton generation, the
    implementation is in large part distributed across other modules, especially:

    - {!module:Adjs} implements state adjacency computation and lookup.
    - {!module:Transit} encapsulates directed transitions between states.
    - {!module:LaneCtx} implements lane tracing.
    - {!module:Contrib} encapsulates conflict contributions.
    - {!module:Attrib} implements symbol-specific attribution of conflict contributions.
    - {!module:Attribs} maps (conflict state, symbol) tuples to conflict attributions.
    - {!module:KernelAttribs} maps kernel items to {!type:Attribs.t} maps.
    - {!module:StateNub} and {!module:Attrib} implement isocore compatibility testing.
    - {!module:GotoNub} and {!module:StateNub} dynamically carry kernel attribs and memoized attribs
      through automaton generation.
*)

open! Basis
open! Basis.Rudiments

(** [gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states] generates a
    function, [gotonub_of_statenub_goto statenub goto], which is used during parser state
    generation. {!type:GotoNub.t} and {!type:StateNub.t} carry cumulative conflict contribution data
    associated with state transitions, which informs state compatibility testing as implemented by
    [StateNub.compat_ielr1] and [Contrib.compat_ielr1]. *)
val gen_gotonub_of_statenub_goto: resolve:bool -> Io.t -> Symbols.t -> Prods.t -> Isocores.t
  -> State.t array -> Io.t * (StateNub.t -> Lr1Itemset.t -> GotoNub.t)
