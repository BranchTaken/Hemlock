(** Symbol-specific attribution of conflict contributions. *)

open Basis
open! Basis.Rudiments

module K : sig
  type t = {
    symbol_index: Symbol.Index.t;
    conflict: Contrib.t;
  }

  include IdentifiableIntf.S with type t := t

  val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

  val init: symbol_index:Symbol.Index.t -> conflict:Contrib.t -> t
end

module V : sig
  type t = {
    ergo_lr1itemset: Lr1Itemset.t;
    contrib: Contrib.t;
  }

  include IdentifiableIntf.S with type t := t

  val equal: t -> t -> bool

  val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
     -> (module Fmt.Formatter)

  val empty: t

  val init: ergo_lr1itemset:Lr1Itemset.t -> contrib:Contrib.t -> t

  val is_empty: t -> bool
  val merge: t -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
end

type t = {
  k: K.t;
  v: V.t;
}

include IdentifiableIntf.S with type t := t

val fmt_hr: Symbols.t -> Prods.t -> ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter)
  -> (module Fmt.Formatter)
