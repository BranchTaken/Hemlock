open Rudiments

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

  val fold_until: init:'accum -> f:('accum -> elm -> 'accum * bool) -> t
    -> 'accum
  (** [fold_until ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value, continuing until [f] returns [accum, true], or
      until folding is complete if [f] always returns [accum, false]. *)

  val fold_right_until: init:'accum -> f:(elm -> 'accum -> 'accum * bool) -> t
    -> 'accum
  (** [fold_right_until ~init ~f t] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: init:'accum -> f:(usize -> 'accum -> elm -> 'accum * bool)
    -> t -> 'accum
  (** [foldi_until ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold: init:'accum -> f:('accum -> elm -> 'accum) -> t -> 'accum
  (** [fold ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val fold_right: init:'accum -> f:(elm -> 'accum -> 'accum) -> t -> 'accum
  (** [fold_right ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: init:'accum -> f:(usize -> 'accum -> elm -> 'accum) -> t -> 'accum
  (** [foldi ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iter: f:(elm -> unit) -> t -> unit
  (** [iter ~f t] iterates from left to right over [t]. *)

  val iteri: f:(usize -> elm -> unit) -> t -> unit
  (** [iter ~f t] iterates with index provided from left to right over [t]. *)

  val count: f:(elm -> bool) -> t -> usize
  (** [count ~f t] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: f:(elm -> bool) -> t -> bool
  (** [for_any ~f t] iterates from left to right over [t] and returns [true] if
      any invocation of [f] returns [true]. *)

  val for_all: f:(elm -> bool) -> t -> bool
  (** [for_all ~f t] iterates from left to right over [t] and returns [true] if
      all invocations of [f] return [true]. *)

  val find: f:(elm -> bool) -> t -> elm option
  (** [find ~f t] iterates from left to right over [t] and returns [Some a] for
      the first element which [f] returns [true], or [None] if [f] always
      returns [false]. *)

  val find_map: f:(elm -> 'a option) -> t -> 'a option
  (** [find_map ~f t] iterates over [t] and returns [Some a] for an element
      which [f] returns [Some a], or [None] if [f] always returns [None]. *)

  val findi: f:(usize -> elm -> bool) -> t -> elm option
  (** [findi ~f t] iterates from left to right over [t] with index provided to
      [f] and returns [Some a] for an element which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: f:(usize -> elm -> 'a option) -> t -> 'a option
  (** [findi_map ~f t] iterates from left to right over [t] with index provided
      to [f] and returns [Some a] for an element which [f] returns [Some a], or
      [None] if [f] always returns [None]. *)

  val min_elm: cmp:(elm -> elm -> Cmp.t) -> t -> elm option
  (** [min_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the leftmost element which always compares as [Cmp.Lt] or [Cmp.Eq], or
      [None] if [t] is empty. *)

  val max_elm: cmp:(elm -> elm -> Cmp.t) -> t -> elm option
  (** [max_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the leftmost element which always compares as [Cmp.Eq] or [Cmp.Gt], or
      [None] if [t] is empty. *)

  val to_list: t -> elm list
  (** [to_list t] folds [t] from right to left as a {!type:elm list}. *)

  val to_list_rev: t -> elm list
  (** [to_list_rev t] folds [t] from left to right as a {!type:elm list}. *)
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

  val mem: elm -> t -> bool
  (** [mem elm t] returns [true] if [elm] is a member of [t]; [false] otherwise.
  *)
end

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

  val fold_until: init:'accum -> f:('accum -> 'a -> 'accum * bool) -> 'a t
    -> 'accum
  (** [fold_until ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value, continuing until [f] returns [accum, true], or
      until folding is complete if [f] always returns [accum, false]. *)

  val fold_right_until: init:'accum -> f:('a -> 'accum -> 'accum * bool) -> 'a t
    -> 'accum
  (** [fold_right_until ~init ~f t] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum * bool)
    -> 'a t -> 'accum
  (** [foldi_until ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold: init:'accum -> f:('accum -> 'a -> 'accum) -> 'a t -> 'accum
  (** [fold ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val fold_right: init:'accum -> f:('a -> 'accum -> 'accum) -> 'a t -> 'accum
  (** [fold_right ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum) -> 'a t
    -> 'accum
  (** [foldi ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iter: f:('a -> unit) -> 'a t -> unit
  (** [iter ~f t] iterates from left to right over [t]. *)

  val iteri: f:(usize -> 'a -> unit) -> 'a t -> unit
  (** [iter ~f t] iterates with index provided from left to right over [t]. *)

  val count: f:('a -> bool) -> 'a t -> usize
  (** [count ~f t] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: f:('a -> bool) -> 'a t -> bool
  (** [for_any ~f t] iterates from left to right over [t] and returns [true] if
      any invocation of [f] returns [true]. *)

  val for_all: f:('a -> bool) -> 'a t -> bool
  (** [for_all ~f t] iterates from left to right over [t] and returns [true] if
      all invocations of [f] return [true]. *)

  val find: f:('a -> bool) -> 'a t -> 'a option
  (** [find ~f t] iterates from left to right over [t] and returns [Some a] for
      the first element which [f] returns [true], or [None] if [f] always
      returns [false]. *)

  val find_map: f:('a -> 'b option) -> 'a t -> 'b option
  (** [find_map ~f t] iterates over [t] and returns [Some b] for an element
      which [f] returns [Some b], or [None] if [f] always returns [None]. *)

  val findi: f:(usize -> 'a -> bool) -> 'a t -> 'a option
  (** [findi ~f t] iterates from left to right over [t] with index provided to
      [f] and returns [Some a] for an element which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: f:(usize -> 'a -> 'b option) -> 'a t -> 'b option
  (** [findi_map ~f t] iterates from left to right over [t] with index provided
      to [f] and returns [Some b] for an element which [f] returns [Some b], or
      [None] if [f] always returns [None]. *)

  val min_elm: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a option
  (** [min_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the first element which always compares as [Cmp.Lt] or [Cmp.Eq], or
      [None] if [t] is empty. *)

  val max_elm: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a option
  (** [max_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the first element which compares as [Cmp.Eq] or [Cmp.Gt], or [None] if
      [t] is empty. *)

  val to_list: 'a t -> 'a list
  (** [to_list t] folds [t] from right to left as a {!type:'a list}. *)

  val to_list_rev: 'a t -> 'a list
  (** [to_list_rev t] folds [t] from left to right as a {!type:'a list}. *)
end

(** {!module:S_poly_fold_gen} is equivalent to {!module:S_poly_fold}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_fold_gen = sig
  type 'a t
  type 'a elm
  val fold_until: init:'accum -> f:('accum -> 'a elm -> 'accum * bool) -> 'a t
    -> 'accum
  val fold_right_until: init:'accum -> f:('a elm -> 'accum -> 'accum * bool)
    -> 'a t -> 'accum
  val foldi_until: init:'accum -> f:(usize -> 'accum -> 'a elm -> 'accum * bool)
    -> 'a t -> 'accum
  val fold: init:'accum -> f:('accum -> 'a elm -> 'accum) -> 'a t -> 'accum
  val fold_right: init:'accum -> f:('a elm -> 'accum -> 'accum) -> 'a t
    -> 'accum
  val foldi: init:'accum -> f:(usize -> 'accum -> 'a elm -> 'accum) -> 'a t
    -> 'accum
  val iter: f:('a elm -> unit) -> 'a t -> unit
  val iteri: f:(usize -> 'a elm -> unit) -> 'a t -> unit
  val count: f:('a elm -> bool) -> 'a t -> usize
  val for_any: f:('a elm -> bool) -> 'a t -> bool
  val for_all: f:('a elm -> bool) -> 'a t -> bool
  val find: f:('a elm -> bool) -> 'a t -> 'a elm option
  val find_map: f:('a elm -> 'b option) -> 'a t -> 'b option
  val findi: f:(usize -> 'a elm -> bool) -> 'a t -> 'a elm option
  val findi_map: f:(usize -> 'a elm -> 'b option) -> 'a t -> 'b option
  val min_elm: cmp:('a elm -> 'a elm -> Cmp.t) -> 'a t -> 'a elm option
  val max_elm: cmp:('a elm -> 'a elm -> Cmp.t) -> 'a t -> 'a elm option
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

  val mem: 'a -> 'a t -> bool
  (** [mem a t] returns [true] if [a] is a member of [t]; [false] otherwise. *)
end

(** {!module:S_poly_mem_gen} is equivalent to {!module:S_poly_mem}, except that
    {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_mem_gen = sig
  type 'a t
  type 'a elm
  val mem: 'a elm -> 'a t -> bool
end

(* Polymorphic container, e.g. (('a, 'cmp) Ordset). *)

(** General functor input interface for polymorphic containers, e.g. {!type:('a,
    'cmp) Ordset}. *)
module type I_poly2 = sig
  type ('a, 'cmp) t
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  module Cursor : sig
    include Cursor_intf.S_poly2_iter
      with type ('a, 'cmp) container := ('a, 'cmp) t
       and type 'a elm := 'a elm
  end
end

(* Polymorphic container, e.g. (('k, 'v, 'cmp) Ordmap). *)

(** General functor input interface for polymorphic containers, e.g. {!type:('k,
    'v, 'cmp) Ordmap}. *)
module type I_poly3 = sig
  type ('k, 'v, 'cmp) t
  (** Container type. *)

  type 'k key
  (** Key type. *)

  type 'v value
  (** Value type. *)

  module Cursor : sig
    include Cursor_intf.S_poly3_iter
      with type ('k, 'v, 'cmp) container := ('k, 'v, 'cmp) t
       and type 'k key := 'k key
       and type 'v value := 'v value
  end
end
