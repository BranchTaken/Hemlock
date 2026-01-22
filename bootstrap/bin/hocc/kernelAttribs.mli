(** Map of per conflict state kernel item conflict attributions. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

module Seq : sig
  type container = t

  include SeqIntf.SMonoDef with type elm = Lr1Item.t * Attribs.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct kernel items in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns kernel attribs with no kernels. *)

val singleton: Lr1Item.t -> Attribs.t -> t
(** [singleton item attribs] returns singleton [item]->[attribs] conflict attributions. *)

val remerge1: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Ordmap.t -> t -> t
(** [remerge1 remergeable_index_map t] creates kernel attribs with all remergeable LR(1) item set
    closure and state nub indexes translated according to [index_map], where keys are the original
    indexes, and values are the reindexed indexes. *)

val reindex: StateIndexMap.t -> t -> t
(** [reindex state_index_map t] creates kernel attribs with all LR(1) item set closure and state nub
    indexes translated according to [state_index_map]. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no kernel items in [t]. *)

val get: Lr1Item.t -> t -> Attribs.t option
(** [get item t] returns the conflict attributions of [item], or [None] if there are no conflict
    attributions on [item]. *)

val amend: Lr1Item.t -> f:(Attribs.t option -> Attribs.t option) -> t -> t
(** [amend item ~f t] returns an incremental derivative of [t] that is equivalent to [t] in all
    conflict attributions except possibly for [item], as determined by the result of [~f
    attribs_opt], where [attribs_opt = Some attribs] indicates [item] is associated with [attribs]
    in [t], and [attribs_opt = None] indicates [item] has no conflict attributions in [t]. The
    result contains a mapping from [item] to [attribs'] if [~f attribs_opt] returns [Some attribs'];
    the result contains no conflict attributions for [item] if [~f attribs_opt] returns [None]. *)

val insert: Lr1Item.t -> Attribs.t -> t -> t
(** [insert item attribs t] inserts the conflict attributions of [attribs] on [item]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of per kernel conflict attributions in [t0] and [t1]. *)

val merge: t -> t -> bool * t
(** [merge t0 t1] computes the union of [t0] and [t1], and returns the result along with a boolean
    indicating whether the union is a strict superset of [t1]. *)

val inter: t -> t -> t
(** [inter t0 t1] returns the intersection of per kernel conflict attributions in [t0] and [t1]. *)

val diff: t -> t -> t
(** [diff t0 t1] returns the set of per kernel conflict contributions present in [t0] but not
    present in [t1]. *)

val attribs: Lr1Itemset.t -> t -> Attribs.t
(** [attribs lr1itemset t] computes the attribs made by [lr1itemset] in the context of [t]. *)

val fold_until: init:'accum -> f:('accum -> Lr1Item.t * Attribs.t -> 'accum * bool) -> t
  -> 'accum
(** [fold ~init ~f t] folds over the (kernel item, attribs) tuples in [t], using [init] as the
    initial accumulator value, continuing until [f] returns [accum, true], or until folding is
    complete if [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> Lr1Item.t * Attribs.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the (kernel item, attribs) tuples in [t], using [init] as the
    initial accumulator value. *)

val for_any: f:(Lr1Item.t * Attribs.t -> bool) -> t -> bool
(** [for_any ~f t] iterates over [t] and returns true if any invocation of [f] returns true, false
    otherwise. *)

val fold2_until: init:'accum -> f:('accum -> (Lr1Item.t * Attribs.t) option
  -> (Lr1Item.t * Attribs.t) option -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel item, attribs) tuples in [t0] and [t1].
    Folding terminates early if [~f] returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> (Lr1Item.t * Attribs.t) option
  -> (Lr1Item.t * Attribs.t) option -> 'accum) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel item, attribs) tuples in [t0] and [t1]. *)
