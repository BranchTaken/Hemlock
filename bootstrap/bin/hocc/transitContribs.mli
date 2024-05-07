(** Conflict contributions associated with a transition. *)

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

val of_anon_contribs: Contribs.t -> t
(** [of_anon_contribs anon_contribs] initializes a {type:t} by inserting [anon_contribs]. *)

val of_anon_contribs_direct: Contribs.t -> t
(** [of_contribs contribs_direct] initializes a {type:t} by inserting [anon_contribs] as direct
    conflict contributions. *)

val reindex: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Map.t -> t -> t
(** [reindex index_map t] creates kernel contribs with all LR(1) item set closure and state nub
    indexes translated according to [index_map], where keys are the original indexes, and values are
    the reindexed indexes. *)

val all: t -> Contribs.t
(** [all t] returns the union of all conflict contributions in [t]. *)

val direct: t -> Contribs.t
(** [direct t] returns the union of direct conflict contributions made by all kernel items. *)

val kernel_contribs: t -> KernelContribs.t
(** [kernel_contribs t] returns the per kernel item reduce conflict contributions in [t]. Shift
    contributions are omitted since it is irrelevant which kernel item makes a shift contribution,
    whether directly or indirectly. *)

val merge: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> conflict:Contrib.t
  -> contrib:Contrib.t -> t -> t
(** [merge ~conflict_state_index ~symbol_index ~conflict ~contrib t] merges the conflict
    contribution [contrib] to state [conflict_state_index] on symbol [symbol_index] into the set of
    all conflict contributions. *)

val merge_direct: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t
  -> conflict:Contrib.t -> contrib:Contrib.t -> t -> t
(** [merge_direct ~conflict_state_index ~symbol_index ~conflict ~contrib t] merges the conflict
    contribution [aval] to state [conflict_state_index] on symbol [symbol_index] into the set of
    direct conflict contributions, as well as into the set of all conflict contributions. *)

val insert_kernel_contribs: KernelContribs.t -> t -> t
(** [insert_kernel_contribs kernel_contribs t] inserts the conflict contributions in
    [kernel_contribs] into the conflict contributions. In addition, merge [kernel_contribs] into
    the set of all conflict contributions. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of transit conflict contributions in [t0] and [t1]. *)

val contribs: Lr1Itemset.t -> t -> Contribs.t
(** [contribs lr1itemset t] computes the contribs made by [lr1itemset] in the context of [t]. *)
