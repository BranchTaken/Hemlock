(** Symbol-specific attribution of conflict contributions. *)

open Basis
open! Basis.Rudiments

type t = {
  conflict_state_index: StateIndex.t; (** State index. *)
  symbol_index: Symbol.Index.t; (** Symbol index. *)
  conflict: Contrib.t;
  (** Conflict on symbol. This is a non-strict superset of attributed conflict contribution, i.e.
      the attribution may not explain the entire conflict. *)
  isucc_lr1itemset: Lr1Itemset.t;
  (** Immediate successor's LR(1) itemset. Empty for lane attribs (maintained only during lane
      tracing). *)
  contrib: Contrib.t; (** Attributed conflict contribution. *)
}

include IdentifiableIntf.S with type t := t

val equal_keys: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] have equal ([conflict_state_index],
    [symbol_index], [conflict]) keys. *)

val remergeable_keys: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] have equal ([conflict_state_index],
    [symbol_index]) keys. This is a weaker condition than [equal_keys], necessary to allow remerging
    of functionally equivalent states despite potential attrib differences. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. The keys must be
    equal. *)

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val empty: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> conflict:Contrib.t
  -> t
(** [empty ~conflict_state_index ~symbol_index ~conflict] returns an empty attribution, i.e. with no
    itemsets nor conflict contributions. *)

val init: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> conflict:Contrib.t
  -> isucc_lr1itemset:Lr1Itemset.t -> contrib:Contrib.t -> t
(** [init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset ~contrib] returns an
    attribution with key (conflict_state_index, symbol_index) that attributes [contrib] to
    [isucc_lr1itemset]. *)

val remerge1: (StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Ordmap.t -> t -> t
(** [remerge1 remergeable_index_map t] creates an attrib with all remergeable state indexes
    translated according to [remergeable_index_map], where keys are the original indexes, and values
    are the reindexed indexes. *)

val reindex: StateIndexMap.t -> t -> t option
(** [reindex state_index_map t] creates an attrib with all state indexes translated according to
    [state_index_map]. If no translation exists, returns [None] to indicate that the attrib is
    obsolete. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attributions in [t]. *)

val union: t -> t -> t
(** [union t0 t1] returns an attribution with the union of attribution values in [t0] and [t1]. The
    keys must be equal. *)

val union_remerged: t -> t -> t
(** [union t0 t1] returns an attribution with the union of attribution values in [t0] and [t1]. The
    keys must be remergeable. *)

val inter: t -> t -> t
(** [inter t0 t1] returns an attribution with the intersection of attribution values in [t0] and
    [t1]. The keys must be equal. *)

val diff: t -> t -> t
(** [diff t0 t1] returns an attrib containing the contents of [t0] that are not in [t1]. *)

val equal_ielr1: resolve:bool -> Symbols.t -> Prods.t -> t -> t -> bool
(** [equal_ielr1 ~resolve symbols prods t0 t1] determines whether [t0] and [t1] make equal
    contributions. *)

val compat_ielr1: resolve:bool -> Symbols.t -> Prods.t -> t -> t -> bool
(** [compat_ielr1 ~resolve symbols prods t0 t1] determines whether [t0] and [t1] make compatible
    contributions. If [resolve] is true, allow conflicts that cannot lead to inadequacy (i.e.
    shift-reduce conflicts cannot lead to inadequacy if the conflict manifestation contains a shift
    action and a single reduce action). *)
