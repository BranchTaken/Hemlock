(** Collection of useful token associativities and token/precedence precedences, used for reporting
    useless associativity/precedence specifications. *)

open! Basis
open! Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val empty: t
(** [empty] returns an empty set of useful resolvers. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of [t0] and [t1]. *)

val use_assoc: PrecSet.Index.t -> t -> t
(** [use_assoc prec_set_index t] returns a derivative of [t] with [prec_set_index] in the set of
    precedence sets with useful associativities. *)

val use_token_prec: Symbol.Index.t -> t -> t
(** [use_token_prec token_index t] returns a derivative of [t] with [token_index] in the set of
    tokens with useful precedence specifications. *)

val use_prod_prec: Prod.Index.t -> t -> t
(** [use_prod_prec prod_index t] returns a derivative of [t] with [prod_index] in the set of
    productions with useful precedence specifications. *)

val is_assoc_useful: PrecSet.Index.t -> t -> bool
(** [is_assoc_useful prec_set_index t] returns true if the associativity of [prec_set_index] was
    useful for conflict resolution. *)

val is_token_prec_useful: Symbol.Index.t -> t -> bool
(** [is_token_prec_useful token_index t] returns true if the precedence of [token_index] was useful
    for conflict resolution. *)

val is_prod_prec_useful: Prod.Index.t -> t -> bool
(** [is_prod_prec_useful prod_index t] returns true if the precedence of [prod_index] was useful for
    conflict resolution. *)
