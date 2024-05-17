(** Conflict attributions associated with a transition. *)

open Basis
open! Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val empty: t
(** [empty] returns an empty {type:t}. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attribs in [t]. *)

val of_attribs_potential: Attribs.t -> t
(** [of_attribs_potential lane_attribs] initializes a {type:t} by inserting [lane_attribs] as
    potential conflict attributions into the set of all conflict attributions. *)

val of_attribs_definite: Attribs.t -> t
(** [of_attribs_definite lane_attribs] initializes a {type:t} by inserting [lane_attribs] as
    definite conflict attributions into the set of definite conflict attributions, as well as into
    the set of all conflict attributions. *)

val reindex: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Map.t -> t -> t
(** [reindex index_map t] creates kernel attribs with all LR(1) item set closure and state nub
    indexes translated according to [index_map], where keys are the original indexes, and values are
    the reindexed indexes. *)

val all: t -> Attribs.t
(** [all t] returns the union of all potential and definite conflict attributions in [t]. *)

val definite: t -> Attribs.t
(** [definite t] returns the union of definite conflict attributions made by all kernel items, as
    well as any definite conflict attributions introduced via [of_attribs_definite] and/or
    [merge_definite]. *)

val kernel_attribs_all: t -> KernelAttribs.t
(** [kernel_attribs_all t] returns the per kernel item reduce conflict attributions in [t]. Shift
    attributions are omitted since it is irrelevant which kernel item has a shift attribution,
    whether definite or potential. *)

val merge_potential: Attrib.t -> t -> t
(** [merge_potential lane_attrib t] merges [lane_attrib] into the set of all conflict attributions.
*)

val merge_definite: Attrib.t -> t -> t
(** [merge_definite lane_attrib t] merges [lane_attrib] into the set of definite conflict
    attributions, as well as into the set of all conflict attributions. *)

val insert_kernel_attribs_all: KernelAttribs.t -> t -> t
(** [insert_kernel_attribs_all kernel_attribs t] inserts the conflict attributions in
    [kernel_attribs] into the set of kernel attribs, as well as into the set of potential conflict
    attributions. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of transit conflict attributions in [t0] and [t1]. *)

val attribs: Lr1Itemset.t -> t -> Attribs.t
(** [attribs lr1itemset t] computes the attribs made by [lr1itemset] in the context of [t]. *)
