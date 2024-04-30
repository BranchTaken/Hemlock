(** Symbol-specific attribution of conflict contributions. Attributions contain keys rather than
    being flat structures, because keys must be used independently of values in some contexts. *)

open Basis
open! Basis.Rudiments

(** Attribution key. *)
module K : sig
  type t = {
    symbol_index: Symbol.Index.t; (** Symbol index. *)
    conflict: Contrib.t; (** Conflict on symbol. This is a non-strict superset of attributed
                             conflict contribution, i.e. the attribution may not explain the entire
                             conflict. *)
  }

  include IdentifiableIntf.S with type t := t

  val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** [pp_hr symbols prods t formatter] pretty-prints a human-readable representation of [t]. *)

  val init: symbol_index:Symbol.Index.t -> conflict:Contrib.t -> t
  (** [init ~symbol_index ~conflict] returns an attribution key for a [conflict] on [symbol_index].
  *)
end

type t = {
  k: K.t; (** Key. *)
  ergo_lr1itemset: Lr1Itemset.t; (** Ergo's LR(1) itemset. *)
  contrib: Contrib.t; (** Attributed conflict contribution. *)
}

include IdentifiableIntf.S with type t := t

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. The keys must be
    equal. *)

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
(** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
    If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
    (elements are indented to [~width + 4]). *)

val empty: k:K.t -> t
(** [empty ~k] returns an empty attribution, i.e. with no itemsets nor contribs. *)

val init: k:K.t -> ergo_lr1itemset:Lr1Itemset.t -> contrib:Contrib.t -> t
(** [init ~k ~ergo_lr1itemset ~contrib] returns an attribution with key [k] that attributes
    [contrib] to [ergo_lr1itemset]. *)

val is_empty: t -> bool
(** [is_empty t] returns true if there are no attributions in [t]. *)

val union: t -> t -> t
(** [union t0 t1] returns an attribution with the union of attribution values in [t0] and [t1]. The
    keys must be equal. *)

val inter: t -> t -> t
(** [inter t0 t1] returns an attribution with the intersection of attribution values in [t0] and
    [t1]. The keys must be equal. *)
