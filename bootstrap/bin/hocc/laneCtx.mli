(** Lane context for tracing conflict attributions backward from a conflict state. A conflicting
    reduce action is typically attributed to one or more states that begin lanes to the
    conflict-inducing kernel item(s) in the conflict state. Conflict attributions are attributed on
    a per symbol basis, even though it is common for one lane to correspond to multiple conflicts.
    It is also possible for multiple items (and therefore distinct lanes) to contribute to the same
    conflict.

    For each state during lane tracing a distinct lane context is created to represent the state's
    role in the lane(s) being traced. For the conflict state the context contains a (symbol, action,
    lr1item) tuple for each kernel item which can lead to the conflicting action on the symbol. This
    may be due to a simple reduction of a kernel item, e.g. `A ::= t B C · {u}`. The more
    complicated case is due to reduction of an added ε production corresponding to one or more
    kernel items with dot positions that are not at the rightmost position, as shown in the
    following example.

    - Contributing state
      A ::= t · B C {⊥}    kernel
      B ::= · D E F {u}    added
    - Interstitial state
      B ::= D · E F {u}    kernel
    - Conflict state
      B ::= D E · F {u}    kernel
      F ::= · u     {u}    added (shift)
      F ::= ·       {u}    added (reduce)

    Note that the relevant item(s) in conflict/interstitial states are always kernel items, whereas
    the relevant item(s) in contributing states are always added items.
*)

open! Basis
open! Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val conflict_state: t -> State.t
(** [conflict_state t] returns the conflict state that [t] leads to. *)

val isucc: t -> State.t
(** [isucc t] returns the state [t] immediately leads to. *)

val state: t -> State.t
(** [state t] returns the state corresponding to [t]. *)

val transit: t -> Transit.t
(** [transit t] returns a transit with source [state t] and destination [isucc t]. *)

val traces_length: t -> uns
(** [traces_length t] returns the number of lane traces in [t]. If [t] contains no traces, its
    predecessors will contain no traces nor conflict attributions. *)

val of_conflict_state: resolve:bool -> Symbols.t -> Prods.t -> State.t -> t
(** [of_conflict_state ~resolve symbols prods conflict_state] creates a lane context for the
    conflict state. *)

val of_ipred: State.t -> t -> t
(** [of_ipred ipred t] creates a lane context for the [ipred] state, where [t] is the lane context
    for the [ipred] state's immediate successor (isucc) state in the lane. *)

val post_init: t list -> t -> t
(** [post_init ipred_lanectxs t] finishes initializing definite lane conflict attributions, given
    all (acyclic) ipreds' contexts and returns a derivative of [t]. *)

val kernel_attribs: t -> KernelAttribs.t
(** [kernel_attribs t] returns a map of the conflict attributions directly attributable to the
    lane(s) encompassing [t], i.e. both definite and potential conflict attributions. *)

val lane_attribs_all: t -> Attribs.t
(** [lane_attribs_all t] returns a map of the merged lane conflict attributions attributable to
    the lanes encompassing [t], i.e. both definite and potential conflict attributions. *)

val lane_attribs_definite: t -> Attribs.t
(** [lane_attribs_definite t] returns a map of the merged lane conflict attributions directly
    attributable to the transition from [state t] to [isucc t], i.e. definitely attributable to
    lanes encompassing [t]. Conflict attributions added by [post_init] are included in the map. *)
