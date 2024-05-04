(** Collection of distinct (conflict state, symbol, contrib) tuples. These are referred to as
    anonymous conflict attributions because they dissociated from their generating LR(1) items. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val fmt: ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt ~alt ~width t formatter] formats a syntactically valid nested list representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

module Seq : sig
  type container = t

  include SeqIntf.SMonoDef with type elm = StateIndex.t * Symbol.Index.t * Contrib.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct (conflict state index, symbol index, contrib) tuples
    in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns contribs with no conflict tuples. *)

val singleton: conflict_state_index:StateIndex.t -> Symbol.Index.t -> Contrib.t -> t
(** [singleton ~conflict_state_index symbol_index contrib] returns a singleton conflict contribution
    collection containing the conflict contribution [contrib] to state [conflict_state_index] on
    symbol [symbol_index]. *)

val reindex: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Map.t -> t -> t
(** [reindex index_map t] creates contribs with all state indexes translated according to
    [index_map], where keys are the original indexes, and values are the reindexed indexes. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no conflict tuples in [t]. *)

val get: conflict_state_index:StateIndex.t -> Symbol.Index.t -> t -> Contrib.t option
(** [get ~conflict_state_index symbol_index t] returns the contrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], [None] otherwise. *)

val get_hlt: conflict_state_index:StateIndex.t -> Symbol.Index.t -> t -> Contrib.t
(** [get_hlt ~conflict_state_index symbol_index t] returns the contrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], halts otherwise. *)

val contains: conflict_state_index:StateIndex.t -> Symbol.Index.t -> Contrib.t -> t -> bool
(** [contains ~conflict_state_index symbol_index contrib t] returns true iff [t] contains a
    non-strict superset of [contrib] for the specified [conflict_state_index] and [symbol_index]. *)

val amend: conflict_state_index:StateIndex.t -> Symbol.Index.t
  -> f:(Contrib.t option -> Contrib.t option) -> t -> t
(** [amend ~conflict_state_index symbol_index ~f t] returns an incremental derivative of [t] that is
    equivalent to [t] in all attributions except possibly for {[conflict_state_index],
    [symbol_index]}, as determined by the result of [~f contrib_opt], where [contrib_opt = Some
    contrib] indicates [symbol_index] is associated with [contrib] in [t], and [contrib_opt = None]
    indicates [symbol_index] is not attributed in [t]. The result contains a mapping from
    [symbol_index] to [contrib'] if [~f contrib_opt] returns [Some contrib']; the result contains no
    attribution for [symbol_index] if [~f contrib_opt] returns [None]. *)

val insert: conflict_state_index:StateIndex.t -> Symbol.Index.t -> Contrib.t -> t -> t
(** [insert ~conflict_state_index symbol_index contrib t] inserts the conflict contribution
    [contrib] to state [conflict_state_index] on symbol [symbol_index]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of conflict contributions in [t0] and [t1]. *)

val fold_until: init:'accum
  -> f:('accum -> StateIndex.t -> Symbol.Index.t -> Contrib.t -> 'accum * bool) -> t -> 'accum
(** [fold ~init ~f t] folds over the (conflict state index, symbol index, contrib) tuples in [t],
    using [init] as the initial accumulator value, continuing until [f] returns [accum, true], or
    until folding is complete if [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> StateIndex.t -> Symbol.Index.t -> Contrib.t -> 'accum) -> t
  -> 'accum
(** [fold ~init ~f t] folds over the (conflict state index, symbol index, contrib) tuples in [t],
    using [init] as the initial accumulator value. The conflict state index is invariant if combined
    with [filter], e.g. [fold ~init ~f (filter ~conflict_state_index t)]. *)

val fold2_until: init:'accum -> f:('accum -> StateIndex.t -> Symbol.Index.t -> Contrib.t option
  -> Contrib.t option -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (conflict state index, symbol index, contrib) tuples
    in [t0] and [t1]. Tuples that differ only in the contrib component are paired in calls to [~f];
    tuples missing from one of the inputs are signified by [None]. Folding terminates early if [~f]
    returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> StateIndex.t -> Symbol.Index.t -> Contrib.t option
  -> Contrib.t option -> 'accum) -> t -> t -> 'accum
(** [fold2 ~init ~f t0 t1] folds over the (conflict state index, symbol index, contrib) tuples in
    [t0] and [t1]. Tuples that differ only in the contrib component are paired in calls to [~f];
    tuples missing from one of the inputs are signified by [None]. *)
