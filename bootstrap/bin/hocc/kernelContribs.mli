(** Map of per kernel item conflict contributions. *)

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

  include SeqIntf.SMonoDef with type elm = Lr1Item.t * Contribs.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct kernel items in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns kernel contribs with no kernels. *)

val singleton: Lr1Item.t -> Contribs.t -> t
(** [singleton item contribs] returns singleton [item]->[contribs] conflict contributions. *)

val reindex: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Map.t -> t -> t
(** [reindex index_map t] creates kernel contribs with all LR(1) item set closure and state nub
    indexes translated according to [index_map], where keys are the original indexes, and values are
    the reindexed indexes. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no kernel items in [t]. *)

val get: Lr1Item.t -> t -> Contribs.t option
(** [get item t] returns the conflict contributions of [item], or [None] if there are no conflict
    contributions on [item]. *)

val amend: Lr1Item.t -> f:(Contribs.t option -> Contribs.t option) -> t -> t
(** [amend item ~f t] returns an incremental derivative of [t] that is equivalent to [t] in all
    conflict contributions except possibly for [item], as determined by the result of [~f
    contribs_opt], where [contribs_opt = Some contribs] indicates [item] is associated with
    [contribs] in [t], and [contribs_opt = None] indicates [item] has no conflict contributions in
    [t]. The result contains a mapping from [item] to [contribs'] if [~f contribs_opt] returns [Some
    contribs']; the result contains no conflict contributions for [item] if [~f contribs_opt]
    returns [None]. *)

val insert: Lr1Item.t -> Contribs.t -> t -> t
(** [insert item contribs t] inserts the conflict contributions of [contribs] on [item]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of per kernel conflict contributions in [t0] and [t1]. *)

val fold_until: init:'accum -> f:('accum -> Lr1Item.t * Contribs.t -> 'accum * bool) -> t
  -> 'accum
(** [fold ~init ~f t] folds over the (kernel item, contribs) tuples in [t], using [init] as the
    initial accumulator value, continuing until [f] returns [accum, true], or until folding is
    complete if [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> Lr1Item.t * Contribs.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the (kernel item, contribs) tuples in [t], using [init] as the
    initial accumulator value. *)

val for_any: f:(Lr1Item.t * Contribs.t -> bool) -> t -> bool
(** [for_any ~f t] iterates over t and returns true if any invocation of f returns true, false
    otherwise. *)

val fold2_until: init:'accum -> f:('accum -> (Lr1Item.t * Contribs.t) option
  -> (Lr1Item.t * Contribs.t) option -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel item, contribs) tuples in [t0] and [t1].
    Folding terminates early if [~f] returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> (Lr1Item.t * Contribs.t) option
  -> (Lr1Item.t * Contribs.t) option -> 'accum) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel item, contribs) tuples in [t0] and [t1]. *)
