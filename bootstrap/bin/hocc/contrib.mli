(** Contribution to a conflict state's conflict, for a particular symbol (which is tracked by an
    enclosing data structure). Contributions come in two flavors:
    - Shift: Conflict states may contain shift actions which conflict with the reduce actions of one
      or more predecessors.
    - Reduce: Conflict states and/or their predecessors may be attributed reduce actions. *)

open! Basis
open! Basis.Rudiments

type t

include IdentifiableIntf.S with type t := t

val equal: t -> t -> bool
(** [equal t0 t1] returns true iff the contents of [t0] and [t1] are identical. *)

val pp_hr: Symbols.t -> Prods.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs contrib in human-readable form. *)

val length: t -> uns
(** [length t] returns the number of contributions (actions) in [t]. *)

val empty: t
(** [empty] returns a contrib with no contributions. *)

val is_empty: t -> bool
(** [is_empty t] returns true if [t] contains no contributions. *)

val shift: t
(** [shift] returns a contrib with shift contribution. *)

val init_reduce: Prod.Index.t -> t
(** [init_reduce prod_index] returns a contrib with reduce contribution corresponding to
    [prod_index]. *)

val mem_shift: t -> bool
(** [mem_shift t] returns true if [t] contains a shift contribution. *)

val reduces: t -> (Prod.Index.t, Prod.Index.cmper_witness) Ordset.t
(** [reduces t] returns the set of production indices corresponding to reduce contributions
    contained by [t]. *)

val union: t -> t -> t
(** [union t0 t1] returns the union of contributions in [t0] and [t1]. *)

val inter: t -> t -> t
(** [inter t0 t1] returns the intersection of contributions in [t0] and [t1]. *)

val diff: t -> t -> t
(** [diff t0 t1] returns the contributions in [t0] that are not in [t1]. *)

val resolve: Symbols.t -> Prods.t -> Symbol.Index.t -> t -> t
(** [resolve symbols prods symbol_index t] returns the resolution of [t] assuming conflict on
    [symbol_index]. *)
