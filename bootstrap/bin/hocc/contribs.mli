(** Collection of distinct (conflict state, attrib) tuples. *)

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

  include SeqIntf.SMonoDef with type elm = StateIndex.t * Attrib.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct (conflict state index, attrib) tuples in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns contribs with no conflict tuples. *)

val singleton: conflict_state_index:StateIndex.t -> Attrib.t -> t
(** [singleton ~conflict_state_index attrib] returns a singleton conflict contribution collection
    containing the conflict contribution [attrib.v] to state [conflict_state_index] on [attrib.k].
*)

val reindex: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Map.t -> t -> t
(** [reindex index_map t] creates contribs with all state indexes translated according to
    [index_map], where keys are the original indexes, and values are the reindexed indexes. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no conflict tuples in [t]. *)

val get: conflict_state_index:StateIndex.t -> Symbol.Index.t -> t -> Attrib.t option
(** [get ~conflict_state_index symbol_index t] returns the attrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], [None] otherwise. *)

val get_hlt: conflict_state_index:StateIndex.t -> Symbol.Index.t -> t -> Attrib.t
(** [get_hlt ~conflict_state_index symbol_index t] returns the attrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], halts otherwise. *)

val contains: conflict_state_index:StateIndex.t -> Symbol.Index.t -> Attrib.V.t -> t -> bool
(** [contains ~conflict_state_index symbol_index aval t] returns true iff [t] contains a non-strict
    superset of [aval] for the specified [conflict_state_index] and [symbol_index]. *)

val amend: conflict_state_index:StateIndex.t -> Attrib.K.t
  -> f:(Attrib.V.t option -> Attrib.V.t option) -> t -> t
(** [amend ~conflict_state_index akey ~f t] returns an incremental derivative of [t] that is
    equivalent to [t] in all attributions except possibly for {[conflict_state_index], [akey]}, as
    determined by the result of [~f aval_opt], where [aval_opt = Some aval] indicates [akey] is
    associated with [aval] in [t], and [aval_opt = None] indicates [akey] is not attributed in [t].
    The result contains a mapping from [akey] to [aval'] if [~f aval_opt] returns [Some aval']; the
    result contains no attribution for [akey] if [~f aval_opt] returns [None]. *)

val insert: conflict_state_index:StateIndex.t -> Attrib.t -> t -> t
(** [insert ~conflict_state_index attrib t] inserts the conflict contribution [attrib.v] to state
    [conflict_state_index] on [attrib.k]. *)

val merged_of_t: t -> t
(** [merged t] returns the merged conflict contributions in [t]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of conflict contributions in [t0] and [t1]. *)

val fold_until: init:'accum -> f:('accum -> StateIndex.t -> Attrib.t -> 'accum * bool) -> t
  -> 'accum
(** [fold ~init ~f t] folds over the (conflict state index, attrib) tuples in [t], using [init]
    as the initial accumulator value, continuing until [f] returns [accum, true], or until folding
    is complete if [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> StateIndex.t -> Attrib.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the (conflict state index, attrib) tuples in [t], using [init]
    as the initial accumulator value. *)

val fold2_until: init:'accum -> f:('accum -> StateIndex.t -> Attrib.K.t -> Attrib.t option
  -> Attrib.t option -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (conflict state index, attrib) tuples in [t0] and
    [t1]. Tuples that differ only in the attrib value component are paired in calls to [~f]; tuples
    missing from one of the inputs are signified by [None]. Folding terminates early if [~f] returns
    [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> StateIndex.t -> Attrib.K.t -> Attrib.t option
  -> Attrib.t option -> 'accum) -> t -> t -> 'accum
(** [fold2 ~init ~f t0 t1] folds over the (conflict state index, akey, aval) tuples in [t0] and
    [t1]. Tuples that differ only in the aval component are paired in calls to [~f]; tuples missing
    from one of the inputs are signified by [None]. *)
