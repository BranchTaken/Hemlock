(** APLR(1) state subgraph remerging functionality. This functionality can also be used as a final
    compaction step for IELR(1) and PGM LR(1), with the caveat that merged IELR(1) conflict
    contributions can be difficult to interpret.

    Although this module drives LR(1) -> APLR(1) transformation the implementation relies on
    facilities distributed across other modules, especially:

    - {!module:Remeargeables} maintains state nub remergeability metadata, fed by paired subgraph
      searches.
    - {!module:StateIndexMap} generates a state index map that informs remerging and reindexing.
    - {!module:Isocores} contains remerging and reindexing logic.
    - {!module:State} and its transitive dependencies ({!module:StateNub}, {!Lr1ItemsetClosure},
      etc.) contain remerging and reindexing logic. *)

open! Basis
open! Basis.Rudiments

val remerge_states: Io.t -> Symbols.t -> Isocores.t -> State.t array
  -> Io.t * Isocores.t * State.t array
(** [remerge_states io symbols isocores states] searches for remergeable state subgraphs in
    [states], then generates a new automaton with all remerges applied. In order for two subgraphs
    to be remergeable, every state pair's pairwise out-transitions must meet at least one of the
    following criteria:

    - The successor state indexes are equal, i.e. no successor state splitting is present.
    - The successor states are transitively remergeable. Note that cycles of arbitrary length may
      occur.
    - State [X₀] takes no action for a symbol [s], and state [X₁] performs only reduction(s) for
      symbol [s]. Furthermore, for more than two states to be remergeable, all additional states
      must contain either no actions on symbol [s], or actions identical to those of state [X₁].
    - State [X₀] contains no goto for a symbol [s], and state [X₁] does contain a goto for symbol
      [s]. Furthermore, for more than two states to be remergeable, all additional states must
      contain either no goto for symbol [s], or a goto successor that is remergeable with that of
      state [X₁]. *)
