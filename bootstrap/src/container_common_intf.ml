open Rudiments

(* Polymorphic container, e.g. ('a list). *)

(** General functor input interface for polymorphic containers, e.g. {!type:'a
    list}. *)
module type I_poly = sig
  type 'a t
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  module Cursor : sig
    include Cursor_intf.S_poly_iter with type 'a container := 'a t
                                     and type 'a elm := 'a elm
  end
end

(** Length-related functor output signature for polymorphic containers, e.g.
    {!type:'a list}. *)
module type S_poly_length = sig
  type 'a t
  (** Container type. *)

  val length: 'a t -> usize
  (** [length t] returns the number of elements in [t]. *)

  val is_empty: 'a t -> bool
  (** [is_empty t] returns [true] if [t] has no elements; [false] otherwise. *)
end

(** {!module:S_poly_length_gen} is equivalent to {!module:S_poly_length}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_length_gen = sig
  type 'a t
  type 'a elm
  val length: 'a t -> usize
  val is_empty: 'a t -> bool
end

(** Folding-related functor output signature for polymorphic containers, e.g.
    {!type:'a list}.  Operations rely on cursor iterators, which should be
    implemented with O(1) iteration to avoid e.g. O(n^2) folding overhead. *)
module type S_poly_fold = sig
  type 'a t
  (** Container type. *)

  val fold_until: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum * bool)
    -> 'accum
  (** [fold_until t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value, continuing until [f] returns [accum, true], or
      until folding is complete if [f] always returns [accum, false]. *)

  val fold_right_until: 'a t -> init:'accum
    -> f:('a -> 'accum -> 'accum * bool) -> 'accum
  (** [fold_right_until t ~init ~f] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: 'a t -> init:'accum
    -> f:(usize -> 'accum -> 'a -> 'accum * bool) -> 'accum
  (** [foldi_until t ~init ~f] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  (** [fold t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val fold_right: 'a t -> init:'accum -> f:('a -> 'accum -> 'accum)
    -> 'accum
  (** [fold_right t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: 'a t -> init:'accum -> f:(usize -> 'accum -> 'a -> 'accum)
    -> 'accum
  (** [foldi t ~init ~f] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iter: 'a t -> f:('a -> unit) -> unit
  (** [iter t ~f] iterates from left to right over [t]. *)

  val iteri: 'a t -> f:(usize -> 'a -> unit) -> unit
  (** [iter t ~f] iterates with index provided from left to right over [t]. *)

  val count: 'a t -> f:('a -> bool) -> usize
  (** [count t ~f] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: 'a t -> f:('a -> bool) -> bool
  (** [for_any t ~f] iterates from left to right over [t] and returns [true] if
      any invocation of [f] returns [true]. *)

  val for_all: 'a t -> f:('a -> bool) -> bool
  (** [for_all t ~f] iterates from left to right over [t] and returns [true] if
      all invocations of [f] return [true]. *)

  val find: 'a t -> f:('a -> bool) -> 'a option
  (** [find t ~f] iterates from left to right over [t] and returns [Some a] for
      the first element which [f] returns [true], or [None] if [f] always
      returns [false]. *)

  val find_map: 'a t -> f:('a -> 'b option) -> 'b option
  (** [find_map t ~f] iterates over [t] and returns [Some b] for an element
      which [f] returns [Some b], or [None] if [f] always returns [None]. *)

  val findi: 'a t -> f:(usize -> 'a -> bool) -> 'a option
  (** [findi t ~f] iterates from left to right over [t] with index provided to
      [f] and returns [Some a] for an element which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: 'a t -> f:(usize -> 'a -> 'b option) -> 'b option
  (** [findi_map t ~f] iterates from left to right over [t] with index provided
      to [f] and returns [Some b] for an element which [f] returns [Some b], or
      [None] if [f] always returns [None]. *)

  val min_elm: 'a t -> cmp:('a -> 'a -> Cmp.t) -> 'a option
  (** [min_elm t ~f] iterates from left to right over [t] and returns [Some a]
      for the first element which always compares as [Cmp.Lt] or [Cmp.Eq], or
      [None] if [t] is empty. *)

  val max_elm: 'a t -> cmp:('a -> 'a -> Cmp.t) -> 'a option
  (** [max_elm t ~f] iterates from left to right over [t] and returns [Some a]
      for the first element which compares as [Cmp.Eq] or [Cmp.Gt], or [None] if
      [t] is empty. *)

  val to_list: 'a t -> 'a list
  (** [to_list t ~f] folds [t] from right to left as a {!type:'a list}. *)

  val to_list_rev: 'a t -> 'a list
  (** [to_list t ~f] folds [t] from left to right as a {!type:'a list}. *)
end

(** {!module:S_poly_fold_gen} is equivalent to {!module:S_poly_fold}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_fold_gen = sig
  type 'a t
  type 'a elm
  val fold_until: 'a t -> init:'accum -> f:('accum -> 'a elm -> 'accum * bool)
    -> 'accum
  val fold_right_until: 'a t -> init:'accum
    -> f:('a elm -> 'accum -> 'accum * bool) -> 'accum
  val foldi_until: 'a t -> init:'accum
    -> f:(usize -> 'accum -> 'a elm -> 'accum * bool) -> 'accum
  val fold: 'a t -> init:'accum -> f:('accum -> 'a elm -> 'accum) -> 'accum
  val fold_right: 'a t -> init:'accum -> f:('a elm -> 'accum -> 'accum)
    -> 'accum
  val foldi: 'a t -> init:'accum -> f:(usize -> 'accum -> 'a elm -> 'accum)
    -> 'accum
  val iter: 'a t -> f:('a elm -> unit) -> unit
  val iteri: 'a t -> f:(usize -> 'a elm -> unit) -> unit
  val count: 'a t -> f:('a elm -> bool) -> usize
  val for_any: 'a t -> f:('a elm -> bool) -> bool
  val for_all: 'a t -> f:('a elm -> bool) -> bool
  val find: 'a t -> f:('a elm -> bool) -> 'a elm option
  val find_map: 'a t -> f:('a elm -> 'b option) -> 'b option
  val findi: 'a t -> f:(usize -> 'a elm -> bool) -> 'a elm option
  val findi_map: 'a t -> f:(usize -> 'a elm -> 'b option) -> 'b option
  val min_elm: 'a t -> cmp:('a elm -> 'a elm -> Cmp.t) -> 'a elm option
  val max_elm: 'a t -> cmp:('a elm -> 'a elm -> Cmp.t) -> 'a elm option
  val to_list: 'a t -> 'a elm list
  val to_list_rev: 'a t -> 'a elm list
end

(** Membership-related functor input interface for polymorphic containers, e.g.
    {!type:'a list}. *)
module type I_poly_mem = sig
  include I_poly

  val cmp_elm: 'a elm -> 'a elm -> Cmp.t
  (** Compare two elements. *)
end

(** Membership-related functor output signature for polymorphic containers, e.g.
    {!type:'a list}. *)
module type S_poly_mem = sig
  type 'a t
  (** Container type. *)

  val mem: 'a t -> 'a -> bool
  (** [mem t a] returns [true] if [a] is a member of [t]; [false] otherwise. *)
end

(** {!module:S_poly_mem_gen} is equivalent to {!module:S_poly_mem}, except that
    {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_mem_gen = sig
  type 'a t
  type 'a elm
  val mem: 'a t -> 'a elm -> bool
end

(* Monomorphic, e.g. string. *)

(** General functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type I_mono = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  module Cursor : sig
    include Cursor_intf.S_mono_iter with type container := t and type elm := elm
  end
end

(** Length-related functor output signature for monomorphic containers, e.g.
    {!type:string}. *)
module type S_mono_length = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val length: t -> usize
  (** [length t] returns the number of elements in [t]. *)

  val is_empty: t -> bool
  (** [is_empty t] returns [true] if [t] has no elements; [false] otherwise. *)
end

(** Folding-related functor output signature for monomorphic containers, e.g.
    {!type:string}.  Operations rely on cursor iterators, which should be
    implemented with O(1) iteration to avoid e.g. O(n^2) folding overhead. *)
module type S_mono_fold = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val fold_until: t -> init:'accum -> f:('accum -> elm -> 'accum * bool)
    -> 'accum
  (** [fold_until t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value, continuing until [f] returns [accum, true], or
      until folding is complete if [f] always returns [accum, false]. *)

  val fold_right_until: t -> init:'accum -> f:(elm -> 'accum -> 'accum * bool)
    -> 'accum
  (** [fold_right_until t ~init ~f] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: t -> init:'accum
    -> f:(usize -> 'accum -> elm -> 'accum * bool) -> 'accum
  (** [foldi_until t ~init ~f] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold: t -> init:'accum -> f:('accum -> elm -> 'accum) -> 'accum
  (** [fold t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val fold_right: t -> init:'accum -> f:(elm -> 'accum -> 'accum) -> 'accum
  (** [fold_right t ~init ~f] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: t -> init:'accum -> f:(usize -> 'accum -> elm -> 'accum) -> 'accum
  (** [foldi t ~init ~f] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iter: t -> f:(elm -> unit) -> unit
  (** [iter t ~f] iterates from left to right over [t]. *)

  val iteri: t -> f:(usize -> elm -> unit) -> unit
  (** [iter t ~f] iterates with index provided from left to right over [t]. *)

  val count: t -> f:(elm -> bool) -> usize
  (** [count t ~f] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: t -> f:(elm -> bool) -> bool
  (** [for_any t ~f] iterates from left to right over [t] and returns [true] if
      any invocation of [f] returns [true]. *)

  val for_all: t -> f:(elm -> bool) -> bool
  (** [for_all t ~f] iterates from left to right over [t] and returns [true] if
      all invocations of [f] return [true]. *)

  val find: t -> f:(elm -> bool) -> elm option
  (** [find t ~f] iterates from left to right over [t] and returns [Some a] for
      the first element which [f] returns [true], or [None] if [f] always
      returns [false]. *)

  val find_map: t -> f:(elm -> 'a option) -> 'a option
  (** [find_map t ~f] iterates over [t] and returns [Some a] for an element
      which [f] returns [Some a], or [None] if [f] always returns [None]. *)

  val findi: t -> f:(usize -> elm -> bool) -> elm option
  (** [findi t ~f] iterates from left to right over [t] with index provided to
      [f] and returns [Some a] for an element which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: t -> f:(usize -> elm -> 'a option) -> 'a option
  (** [findi_map t ~f] iterates from left to right over [t] with index provided
      to [f] and returns [Some a] for an element which [f] returns [Some a], or
      [None] if [f] always returns [None]. *)

  val min_elm: t -> cmp:(elm -> elm -> Cmp.t) -> elm option
  (** [min_elm t ~f] iterates from left to right over [t] and returns [Some a]
      for the first element which always compares as [Cmp.Lt] or [Cmp.Eq], or
      [None] if [t] is empty. *)

  val max_elm: t -> cmp:(elm -> elm -> Cmp.t) -> elm option
  (** [max_elm t ~f] iterates from left to right over [t] and returns [Some a]
      for the first element which compares as [Cmp.Eq] or [Cmp.Gt], or [None] if
      [t] is empty. *)

  val to_list: t -> elm list
  (** [to_list t ~f] folds [t] from right to left as a {!type:elm list}. *)

  val to_list_rev: t -> elm list
  (** [to_list t ~f] folds [t] from left to right as a {!type:elm list}. *)
end

(** Membership-related functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type I_mono_mem = sig
  include I_mono

  val cmp_elm: elm -> elm -> Cmp.t
  (** Compare two elements. *)
end

module type S_mono_mem = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val mem: t -> elm -> bool
  (** [mem t elm] returns [true] if [elm] is a member of [t]; [false] otherwise.
      *)
end
