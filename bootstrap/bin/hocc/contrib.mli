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
(** [length t] returns the number of conflicts [t] contributes to. *)

val empty: t
(** [empty] returns a contrib with no conflicts contributed to. *)

val is_empty: t -> bool
(** [is_empty t] returns true if [t] contributes to no conflicts. *)

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

val stable: resolve:bool -> Symbols.t -> Prods.t -> Symbol.Index.t -> t -> bool
(** [stable ~resolve symbols prods symbol_index t] determines whether the indirect contribution
    represented by [t] is split-stable, i.e. all possible partitions of contributions have the same
    resolution. If [resolve] is false, conflict resolution is not performed prior to evaluating
    contribution partitions, thus impeding split-stability. *)

val compat_ielr1: resolve:bool -> Symbols.t -> Prods.t -> Symbol.Index.t -> t -> t -> bool
(** [compat_ielr1 ~resolve symbols prods symbol_index t0 t1] determines whether [t0] and [t1] make
    compatible contributions, where [symbol_index] is the index of the input symbol (i.e. the symbol
    of any shift action) in [symbols] and any reduce actions have corresponding productions in
    [prods]. If [resolve] is true, allow conflicts that cannot lead to inadequacy (i.e. shift-reduce
    conflicts cannot lead to inadequacy if [conflict_state] contains a shift action and a single
    reduce action for [symbol_index]). *)
