(** Symbol-specific attribution of conflict contributions. Attributions contain keys rather than
    being flat structures, because keys must be used independent of values in some contexts. *)

open Basis
open! Basis.Rudiments

(** Attribution key. *)
module K : sig
  type t = {
    (* Symbol index. *)
    symbol_index: Symbol.Index.t;
    (* Attributed conflict action(s). *)
    conflict: Contrib.t;
  }

  include IdentifiableIntf.S with type t := t

  val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** [pp_hr symbols prods t formatter] pretty-prints a human-readable representation of [t]. *)

  val init: symbol_index:Symbol.Index.t -> conflict:Contrib.t -> t
  (** [init ~symbol_index ~conflict] returns an attribution key for a [conflict] on [symbol_index].
  *)
end

(** Attribution value. *)
module V : sig
  type t = {
    (* Ergo's LR(1) itemset. *)
    ergo_lr1itemset: Lr1Itemset.t;
    (* Conflict contribution. *)
    contrib: Contrib.t;
  }

  include IdentifiableIntf.S with type t := t

  val equal: t -> t -> bool
  (** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

  val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
    -> (module Fmt.Formatter)
  (** [fmt_hr symbols prods ~alt ~width t formatter] formats a human-readable representation of [t].
      If [~alt=true], the output is broken across multiple lines with outermost indentation [~width]
      (elements are indented to [~width + 4]). *)

  val empty: t
  (** [empty] returns an empty attribution value, i.e. with no itemsets nor contribs. *)

  val init: ergo_lr1itemset:Lr1Itemset.t -> contrib:Contrib.t -> t
  (** [init ergo_lr1itemset contrib] returns an attribution value that attributes [contrib] to
      [ergo_lr1itemset]. *)

  val is_empty: t -> bool
  (** [is_empty t] returns true if there are no attributions in [t]. *)

  val union: t -> t -> t
  (** [union t0 t1] returns the union of attributions in [t0] and [t1]. *)

  val inter: t -> t -> t
  (** [inter t0 t1] returns the intersection of attributions in [t0] and [t1]. *)
end

type t = {
  k: K.t;
  v: V.t;
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

val init: k:K.t -> v:V.t -> t
(** [init ~k ~v] returns an attribution with key [k] and value [v]. *)

val union: t -> t -> t
(** [union t0 t1] returns an attribution with the union of attribution values in [t0] and [t1]. The
    keys must be equal. *)
