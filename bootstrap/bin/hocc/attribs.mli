(** Map of per symbol conflict contributions, i.e. attributions of conflict contributions on
    symbols. *)

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

  include SeqIntf.SMonoDef with type elm = Attrib.K.t * (Attrib.K.t * Attrib.V.t)

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of distinct symbol indices in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns attribs with no attributions. *)

val singleton: Attrib.K.t -> Attrib.V.t -> t
(** [singleton akey aval] returns a singleton [akey]->[aval] attribution. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attributions in [t]. *)

val get: Symbol.Index.t -> t -> (Attrib.K.t * Attrib.V.t) option
(** [get symbol_index t] returns the contributions of [symbol_index], or [None] if there are no
    attributions on [symbol_index]. *)

val amend: Attrib.K.t -> f:(Attrib.V.t option -> Attrib.V.t option) -> t -> t
(** [amend akey ~f t] returns an incremental derivative of [t] that is equivalent to [t] in
    all attributions except possibly for [symbol_index], as determined by the result of [~f
    aval_opt], where [aval_opt = Some aval] indicates [symbol_index] is associated with [aval] in
    [t], and [aval_opt = None] indicates [symbol_index] is not attributed in [t]. The result
    contains a mapping from [symbol_index] to [aval'] if [~f aval_opt] returns [Some aval']; the
    result contains no attribution for [symbol_index] if [~f aval_opt] returns [None]. *)

val insert: Attrib.K.t -> Attrib.V.t -> t -> t
(** [insert akey aval t] inserts the attribution of [aval] on [akey]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of attributions in [t0] and [t1]. *)

val fold_until: init:'accum -> f:('accum -> Attrib.K.t * Attrib.V.t -> 'accum * bool) -> t -> 'accum
(** [fold ~init ~f t] folds over the (akey, aval) tuples in [t], using [init] as the initial
    accumulator value, continuing until [f] returns [accum, true], or until folding is complete if
    [f] always returns [accum, false]. *)

val fold: init:'accum -> f:('accum -> Attrib.K.t * Attrib.V.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the (akey, aval) tuples in [t], using [init] as the initial
    accumulator value. *)

val for_any: f:(Attrib.K.t * Attrib.V.t -> bool) -> t -> bool
(** [for_any ~f t] iterates over t and returns true if any invocation of f returns true, false
    otherwise. *)

val fold2_until: init:'accum -> f:('accum -> (Attrib.K.t * Attrib.V.t) option -> (Attrib.K.t * Attrib.V.t) option
  -> 'accum * bool) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (akey, aval) tuples in [t0] and [t1]. Folding
    terminates early if [~f] returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> (Attrib.K.t * Attrib.V.t) option
  -> (Attrib.K.t * Attrib.V.t) option -> 'accum) -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the (akey, aval) tuples in [t0] and [t1]. *)

val symbol_indexes: t -> (Symbol.Index.t, Symbol.Index.cmper_witness) Ordset.t
(** [symbol_indexes t] returns the attributed symbol indexes in [t]. *)
