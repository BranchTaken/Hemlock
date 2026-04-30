(** Conflict contribution attributions map, i.e. (conflict state, symbol) -> attrib. *)

open Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val fmt: ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt ~alt ~width t formatter] formats a syntactically valid nested list representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val fmt_hr: Precs.t -> Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt_hr precs symbols prods ~alt ~width t formatter] formats a human-readable representation of
    [t]. If [~alt=true], the output is broken across multiple lines with outermost indentation
    [~width] (elements are indented to [~width + 4]). *)

module Seq : sig
  type container = t

  include SeqIntf.SMonoDef with type elm = Attrib.t

  val init: container -> t
end

val length: t -> uns
(** [length t] returns the number of conflict attributions in [t]. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val empty: t
(** [empty] returns attribs with no attributions. *)

val singleton: Attrib.t -> t
(** [singleton attrib] returns a singleton conflict attribution collection containing the conflict
    attribution [attrib.v] to state [conflict_state_index] on [attrib.k]. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attributions in [t]. *)

val get: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> t -> Attrib.t option
(** [get ~conflict_state_index ~symbol_index t] returns the attrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], [None] otherwise. *)

val get_hlt: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> t -> Attrib.t
(** [get_hlt ~conflict_state_index ~symbol_index t] returns the attrib for the specified
    [conflict_state_index] and [symbol_index] if present in [t], halts otherwise. *)

val insert: Attrib.t -> t -> t
(** [insert attrib t] inserts the conflict contribution [attrib] to state
    [attrib.conflict_state_index] on [attrib.symbol_index]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of conflict attributions in [t0] and [t1]. *)

val inter: t -> t -> t
(** [inter t0 t1] returns the intersection of conflict attributions in [t0] and [t1]. *)

val diff: t -> t -> t
(** [diff t0 t1] returns the conflict attributions present in [t0] but not present in [t1]. *)

val fold_until: init:'accum -> f:('accum -> Attrib.t -> 'accum * bool) -> t -> 'accum
(** [fold ~init ~f t] folds over the attribs in [t], using [init] as the initial accumulator value,
    continuing until [f] returns [accum, true], or until folding is complete if [f] always returns
    [accum, false]. *)

val fold: init:'accum -> f:('accum -> Attrib.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] folds over the attribs in [t], using [init] as the initial accumulator value.
*)

val for_any: f:(Attrib.t -> bool) -> t -> bool
(** [for_any ~f t] iterates over [t] and returns true if any invocation of [f] returns true, false
    otherwise. *)

val fold2_until: init:'accum -> f:('accum -> Attrib.t option -> Attrib.t option -> 'accum * bool)
  -> t -> t -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the attribs in [t0] and [t1]. Folding terminates early
    if [~f] returns [(_, true)]. *)

val fold2: init:'accum -> f:('accum -> Attrib.t option -> Attrib.t option -> 'accum) -> t -> t
  -> 'accum
(** [fold2_until ~init ~f t0 t1] folds over the attribs in [t0] and [t1]. *)
