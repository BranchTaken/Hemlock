open Rudiments

(** Set.  Note that O(1) time complexity for various unordered set operations
    assumes a collision-free hash function; beware that a degenerate hash
    function which collides for all inputs results in O(n) time complexity. *)
module type S = sig

  (** {1 Types} *)

  type ('a, 'cmp) t
  (** Set type. *)

  type ('a, 'cmp) cmper =
    (module Cmper.S_mono with type t = 'a and type cmper_witness = 'cmp)
  (** Comparator type. *)

  (** {1 Comparators} *)

  val hash_fold: ('a, 'cmp) t -> Hash.State.t -> Hash.State.t
  (** [hash_fold t state] incorporates the hash of [t] into [state] and returns
      the resulting state.  Set members are stably hash-folded into the
      resulting state via the type's comparator [hash_fold] function. *)

  val cmper_m: ('a, 'cmp) t -> ('a, 'cmp) cmper
  (** [cmper_m t] returns a first class module that can be used to build other
      sets or maps with compatible comparison. *)

  val cmper: ('a, 'cmp) t -> ('a, 'cmp) Cmper.t
  (** [cmper t] returns the comparator associated with [t]. *)

  (** {1 Creation} *)

  val empty: ('a, 'cmp) cmper -> ('a, 'cmp) t
  (** [empty cmper] creates an empty set associated with [cmper], which is a
      first-class module encapsulating a comparator. *)

  val singleton: ('a, 'cmp) cmper -> 'a -> ('a, 'cmp) t
  (** [singleton cmper elm] creates a set associated with [cmper] that contains
      [elm]. *)

  val of_list: ('a, 'cmp) cmper -> 'a list -> ('a, 'cmp) t
  (** [of_list cmper elms] creates a set associated with [cmper] that contains
      [elms]. *)

  (** {1 Length} *)

  val length: ('a, 'cmp) t -> usize
  (** [length t] returns the number of elements in [t].  O(1) time complexity.
  *)

  val is_empty: ('a, 'cmp) t -> bool
  (** [is_empty t] returns [true] if [t] has no elements; [false] otherwise.
      O(1) time complexity. *)

  (** {1 Element operations} *)

  val mem: 'a -> ('a, 'cmp) t -> bool
  (** [mem a t] returns [true] if [a] is a member of [t]; [false] otherwise.
      O(lg n) time complexity if ordered, O(1) time complexity if unordered. *)

  val choose: ('a, 'cmp) t -> 'a option
  (** [choose t] returns an arbitrary member of [t] if the set is non-empty,
      [None] otherwise. *)

  val choose_hlt: ('a, 'cmp) t -> 'a
  (** [choose_hlt t] returns an arbitrary member of [t] if the set is non-empty,
      halts otherwise. *)

  val insert: 'a -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [insert elm t] returns a set that is equivalent to the union of a
      singleton set containing [elm] with [t].  O(lg n) time complexity if
      ordered, O(1) time complexity if unordered. *)

  val remove: 'a -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [remove a t] returns a set that is equivalent to the difference of [t]
      relative to the singleton set containing [a].  O(lg n) time complexity if
      ordered, O(1) time complexity if unordered. *)

  (** {1 Set operations} *)

  val equal: ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** [equal t0 t1] returns [true] if [t0] and [t1] contain identical sets of
      elements, [false] otherwise.  O(n) time complexity. *)

  val subset: ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** [subset t0 t1] returns [true] if all elements in [t1] are also in [t0],
      [false] otherwise.  O(n) time complexity. *)

  val disjoint: ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** [disjoint t0 t1] returns [true] if [t0] and [t1] contain disjoint sets of
      elements, [false] otherwise.  O(n) time complexity. *)

  val union: ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [union t0 t1] creates a set that is the union of [t0] and [t1]; that is, a
      set that contains all elements in [t0] or [t1].  O(m lg (n/m + 1)) time
      complexity if ordered, where m and n are the input set lengths and m <= n;
      Θ(m+n) time complexity if unordered. *)

  val inter: ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [inter t0 t1] creates a set that is the intersection of [t0] and [t1];
      that is, a set that contains all elements present in both [t0] and [t1].
      O(m lg (n/m + 1)) time complexity if ordered, where m and n are the input
      set lengths and m <= n; Θ(m+n) time complexity if unordered. *)

  val diff: ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [diff t0 t1] creates a set that is the difference of t0 relative to t1;
      that is, a set that contains all elements present in [t0] but not present
      in [t1].  O(m lg (n/m + 1)) time complexity if ordered, where m and n are
      the input set lengths and m <= n; Θ(m+n) time complexity if unordered. *)

  (** {1 Folding, mapping, filtering, and reducing} *)

  val fold_until: init:'accum -> f:('accum -> 'a -> 'accum * bool)
    -> ('a, 'cmp) t -> 'accum
  (** [fold_until ~init ~f t] folds [t] from left to right if ordered, or
      arbitrarily if unordered, using [init] as the initial accumulator value,
      continuing until [f] returns [accum, true], or until folding is complete
      if [f] always returns [accum, false]. *)

  val fold: init:'accum -> f:('accum -> 'a -> 'accum) -> ('a, 'cmp) t -> 'accum
  (** [fold ~init ~f t] folds [t] from left to right if ordered, or arbitrarily
      if unordered, using [init] as the initial accumulator value. *)

  val iter: f:('a -> unit) -> ('a, 'cmp) t -> unit
  (** [iter ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t]. *)

  val count: f:('a -> bool) -> ('a, 'cmp) t -> usize
  (** [count ~f t] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: f:('a -> bool) -> ('a, 'cmp) t -> bool
  (** [for_any ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [true] if any invocation of [f] returns
      [true]. *)

  val for_all: f:('a -> bool) -> ('a, 'cmp) t -> bool
  (** [for_all ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [true] if all invocations of [f] return
      [true]. *)

  val find: f:('a -> bool) -> ('a, 'cmp) t -> 'a option
  (** [find ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [Some a] for the first element which [f]
      returns [true], or [None] if [f] always returns [false]. *)

  val find_map: f:('a -> 'b option) -> ('a, 'cmp) t -> 'b option
  (** [find_map ~f t] iterates over [t] and returns [Some b] for an element
      which [f] returns [Some b], or [None] if [f] always returns [None]. *)

  val filter: f:('a -> bool) -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [filter ~f t] creates a set with contents filtered by [~f].  Only elements
      for which the filter function returns [true] are incorporated into the
      result. *)

  val partition_tf: f:('a -> bool) -> ('a, 'cmp) t
    -> ('a, 'cmp) t * ('a, 'cmp) t
  (** [partition_tf ~f t] partitions [t] into two sets for which [~f] returns
      [true] vs [false]. *)

  val reduce: f:('a -> 'a -> 'a) -> ('a, 'cmp) t -> 'a option
  (** [reduce ~f t] reduces [t] to a single value, or [None] if the set is
      empty.  The reduction function is assumed to be associative; thus
      reduction order is unspecified. *)

  val reduce_hlt: f:('a -> 'a -> 'a) -> ('a, 'cmp) t -> 'a
  (** [reduce_hlt ~f t] reduces [t] to a single value, or halts if the set is
      empty.  The reduction function is assumed to be associative; thus
      reduction order is unspecified. *)

  include Seq_intf.S_poly2_fold2
    with type ('a, 'cmp) t := ('a, 'cmp) t
     and type 'a elm := 'a

  (** {1 Conversion} *)

  val to_list: ('a, 'cmp) t -> 'a list
  (** [to_list t] folds [t] from right to left if ordered, or arbitrarily if
      unordered, as a {!type:'a list}. *)

  include Container_array_intf.S_poly2_array
    with type ('a, 'cmp) t := ('a, 'cmp) t
end

(** Ordered set. *)
module type S_ord = sig
  include S

  (** {1 Creation} *)

  val of_array: ('a, 'cmp) cmper -> 'a array -> ('a, 'cmp) t
  (** [of_array cmper elms] creates a set associated with [cmper] that contains
      [elms]. *)

  (** {1 Cursor} *)

  (** Cursor that supports arbitrary set member access.  [hd], [tl], [seek],
      [succ], and [pred] are O(lg n), but complete traversals via [succ] or
      [pred] are amortized O(1) per call.  [lget], [rget], [container], and
      [index] are O(1). *)
  module Cursor : sig
    type ('a, 'cmp) container = ('a, 'cmp) t
    type ('a, 'cmp) t

    include Cursor_intf.S_poly2
      with type ('a, 'cmp) container := ('a, 'cmp) container
      with type 'a elm := 'a
      with type ('a, 'cmp) t := ('a, 'cmp) t
  end

  (** {1 Element operations} *)

  val nth_opt: usize -> ('a, 'cmp) t -> 'a option
  (** [nth i t] returns the nth set element (0-indexed), or [None] if [i] is out
      of bounds. *)

  val nth: usize -> ('a, 'cmp) t -> 'a
  (** [nth i t] returns the nth set element (0-indexed), or halts if [i] is out
      of bounds. *)

  val psearch: 'a -> ('a, 'cmp) t -> (Cmp.t * usize) option
  (** [psearch a t] searches for [a] in [t], and falls back to the nearest
      present predecessor of [a] in the case of no match.
      @return {ul
        {- No predecessor: [Some (Cmp.Lt, 0)]}
        {- Leftmost match: [Some (Cmp.Eq, index)]}
        {- Predecessor: [Some (Cmp.Gt, index)]}
        {- Empty set: [None]}
      } *)

  val search: 'a -> ('a, 'cmp) t -> usize option
  (** [search a t] returns [(Some index)] if [a] is a member of [t]; [None]
      otherwise.  O(lg n) time complexity if ordered, O(1) time complexity if
      unordered. *)

  val nsearch: 'a -> ('a, 'cmp) t -> (Cmp.t * usize) option
  (** [nsearch a t] searches for [a] in [t], and falls back to the nearest
      present succesor of [a] in the case of no match.
      @return {ul
        {- Successor: [Some (Cmp.Lt, index)]}
        {- Match: [Some (Cmp.Eq, index)]}
        {- No successor: [Some (Cmp.Gt, (Usize.pred (length t)))]}
        {- Empty set: [None]}
      } *)

  (** {1 Set operations} *)

  val cmp: ('a, 'cmp) t -> ('a, 'cmp) t -> Cmp.t
  (** [cmp t0 t1] compares [t0] and [t1].  O(m+n) time complexity, where m and n
      are the input set lengths. *)

  val split: 'a -> ('a, 'cmp) t -> ('a, 'cmp) t * 'a option * ('a, 'cmp) t
  (** [split a t] tripartitions [t] into elements \{<,=,>\} [a], respectively.
  *)

  (** {1 Folding, mapping, and filtering} *)

  val fold_right_until: init:'accum -> f:('a -> 'accum -> 'accum * bool)
    -> ('a, 'cmp) t -> 'accum
  (** [fold_right_until ~init ~f t] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum * bool)
    -> ('a, 'cmp) t -> 'accum
  (** [foldi_until ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold_right: init:'accum -> f:('a -> 'accum -> 'accum) -> ('a, 'cmp) t
    -> 'accum
  (** [fold_right ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum) -> ('a, 'cmp) t
    -> 'accum
  (** [foldi ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iteri: f:(usize -> 'a -> unit) -> ('a, 'cmp) t -> unit
  (** [iter ~f t] iterates with index provided from left to right over [t]. *)

  val findi: f:(usize -> 'a -> bool) -> ('a, 'cmp) t -> 'a option
  (** [findi ~f t] iterates from left to right over [t] with index provided to
      [f] and returns [Some a] for an element which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: f:(usize -> 'a -> 'b option) -> ('a, 'cmp) t -> 'b option
  (** [findi_map ~f t] iterates from left to right over [t] with index provided
      to [f] and returns [Some b] for an element which [f] returns [Some b], or
      [None] if [f] always returns [None]. *)

  val filteri: f:(usize -> 'a -> bool) -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [filter ~f t] creates a set with contents filtered by [~f].  Only elements
      for which the filter function returns [true] are incorporated into the
      result. *)

  val partitioni_tf: f:(usize -> 'a -> bool) -> ('a, 'cmp) t
    -> ('a, 'cmp) t * ('a, 'cmp) t
  (** [partitioni_tf ~f t] partitions [t] into two sets for which [~f] returns
      [true] vs [false]. *)

  val min_elm: cmp:('a -> 'a -> Cmp.t) -> ('a, 'cmp) t -> 'a option
  (** [min_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the leftmost element which always compares as [Cmp.Lt] or [Cmp.Eq], or
      [None] if [t] is empty. *)

  val max_elm: cmp:('a -> 'a -> Cmp.t) -> ('a, 'cmp) t -> 'a option
  (** [max_elm ~f t] iterates from left to right over [t] and returns [Some a]
      for the leftmost element which always compares as [Cmp.Eq] or [Cmp.Gt], or
      [None] if [t] is empty. *)

  (** {1 Conversion} *)

  val to_list_rev: ('a, 'cmp) t -> 'a list
  (** [to_list_rev t] folds [t] from left to right as a {!type:'a list}. *)
end
