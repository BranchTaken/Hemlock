(** Symbol-specific attribution of conflict contributions. *)

open Basis
open! Basis.Rudiments

type t = {
  conflict_state_index: StateIndex.t; (** State index. *)
  symbol_index: Symbol.Index.t; (** Symbol index. *)
  conflict: Contrib.t;
  (** Conflict on symbol. This is a non-strict superset of attributed conflict contribution, i.e.
      the attribution may not explain the entire conflict. *)
  isucc_lr1itemset: Lr1Itemset.t; (** Immediate successor's LR(1) itemset. *)
  contrib: Contrib.t; (** Attributed conflict contribution. *)
}

include IdentifiableIntf.S with type t := t

val equal_keys: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] have equal ([conflict_state_index],
    [symbol_index], [conflict]) keys. *)

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. The keys must be
    equal. *)

val fmt_hr: Precs.t -> Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt_hr precs symbols prods ~alt ~width t formatter] formats a human-readable representation of
    [t]. If [~alt=true], the output is broken across multiple lines with outermost indentation
    [~width] (elements are indented to [~width + 4]). *)

val init: conflict_state_index:StateIndex.t -> symbol_index:Symbol.Index.t -> conflict:Contrib.t
  -> isucc_lr1itemset:Lr1Itemset.t -> contrib:Contrib.t -> t
(** [init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset ~contrib] returns an
    attribution with key (conflict_state_index, symbol_index) that attributes [contrib] to
    [isucc_lr1itemset]. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attributions in [t]. *)

val union: t -> t -> t
(** [union t0 t1] returns an attribution with the union of attribution values in [t0] and [t1]. The
    keys must be equal. *)

val inter: t -> t -> t
(** [inter t0 t1] returns an attribution with the intersection of attribution values in [t0] and
    [t1]. The keys must be equal. *)

val diff: t -> t -> t
(** [diff t0 t1] returns an attrib containing the contents of [t0] that are not in [t1]. *)

val compat_ielr: resolve:bool -> Precs.t -> Symbols.t -> Prods.t -> t -> t -> bool
(** [compat_ielr ~resolve precs symbols prods t0 t1] merges shift contribs into the contribs of [t0]
    and [t1] if the conflict manifestation contains shift, returns true if at least one contrib is
    empty (oblivious to merging and therefore compatible with any other contrib), performs conflict
    resolution on the contribs if [resolve] is true, returns true iff the (optionally resolved)
    contribs are equal. *)

val compat_ielr_implicit: resolve:bool -> Precs.t -> Symbols.t -> Prods.t -> t -> bool
(** [compat_ielr_implicit ~resolve precs symbols prods t] merges shift contribs into the contribs of
    [t] and an implicit attrib (i.e. no reduces attributed) if the conflict manifestation contains
    shift, returns true if at least one contrib is empty (oblivious to merging and therefore
    compatible with any other contrib), performs conflict resolution on the contribs if [resolve] is
    true, returns true iff the (optionally resolved) contribs are equal. *)
