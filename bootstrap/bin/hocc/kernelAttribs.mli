(** Map of per conflict state kernel lr0item conflict attributions. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val fmt_hr: Precs.t -> Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt_hr precs symbols prods ~alt ~width t formatter] formats a human-readable representation of
    [t]. If [~alt=true], the output is broken across multiple lines with outermost indentation
    [~width] (elements are indented to [~width + 4]). *)

module Seq : sig
  type container = t

  include SeqIntf.SMonoDef with type elm = Lr0Item.t * Attribs.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct kernel items in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns kernel attribs with no kernels. *)

val singleton: Lr0Item.t -> Attribs.t -> t
(** [singleton lr0item attribs] returns singleton [lr0item]->[attribs] conflict attributions. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no kernel items in [t]. *)

val get: Lr0Item.t -> t -> Attribs.t option
(** [get lr0item t] returns the conflict attributions of [lr0item], or [None] if there are no
    conflict attributions on [lr0item]. *)

val amend: Lr0Item.t -> f:(Attribs.t option -> Attribs.t option) -> t -> t
(** [amend item ~f t] returns an incremental derivative of [t] that is equivalent to [t] in all
    conflict attributions except possibly for [item], as determined by the result of [~f
    attribs_opt], where [attribs_opt = Some attribs] indicates [item] is associated with [attribs]
    in [t], and [attribs_opt = None] indicates [item] has no conflict attributions in [t]. The
    result contains a mapping from [item] to [attribs'] if [~f attribs_opt] returns [Some attribs'];
    the result contains no conflict attributions for [item] if [~f attribs_opt] returns [None]. *)

val insert: Lr0Item.t -> Attribs.t -> t -> t
(** [insert lr0item attribs t] inserts the conflict attributions of [attribs] on [lr0item]. *)

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

val goto_attribs: Lr1Itemset.t -> t -> t * Attribs.t
(** [goto_attribs kernel t] computes the immediate successor kernel attribs made by goto [kernel] in
    the context of [t] (which is associated with the relevant ipred -> [kernel] transit) and returns
    the result both as kernel attribs and flattened attribs (i.e. union of kernel attribs). *)

val fold_until: init:'accum -> f:('accum -> Lr0Item.t * Attribs.t -> 'accum * bool) -> t -> 'accum
(** [fold ~init ~f t] folds over the (kernel lr0item, attribs) tuples in [t], using [init] as the
    initial accumulator value, continuing until [f] returns [accum, true], or until folding is
    complete if [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> Lr0Item.t * Attribs.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the (kernel lr0item, attribs) tuples in [t], using [init] as the
    initial accumulator value. *)

val for_any: f:(Lr0Item.t * Attribs.t -> bool) -> t -> bool
(** [for_any ~f t] iterates over [t] and returns true if any invocation of [f] returns true, false
    otherwise. *)

val fold2_until: init:'accum -> f:('accum -> (Lr0Item.t * Attribs.t) option
  -> (Lr0Item.t * Attribs.t) option -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel lr0item, attribs) tuples in [t0] and [t1].
    Folding terminates early if [~f] returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> (Lr0Item.t * Attribs.t) option
  -> (Lr0Item.t * Attribs.t) option -> 'accum) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (kernel lr0item, attribs) tuples in [t0] and [t1].
*)
