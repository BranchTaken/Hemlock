open Rudiments

(** Map.  Note that O(1) time complexity for various unordered map operations
    assumes a collision-free hash function; beware that a degenerate hash
    function which collides for all inputs results in O(n) time complexity. *)
module type S = sig
  (** {1 Types} *)

  type ('k, 'v, 'cmp) t
  (** Map type. *)

  type ('k, 'cmp) cmper =
    (module Cmper.S_mono with type t = 'k and type cmper_witness = 'cmp)
  (** Comparator type. *)

  (** {1 Seq} *)

  (** Seq that supports arbitrarily ordered sequential access. *)
  module Seq : sig
    type ('k, 'v, 'cmp) container = ('k, 'v, 'cmp) t
    type ('k, 'v, 'cmp) t

    include Seq_intf.I_poly3_fold2
      with type ('k, 'v, 'cmp) container := ('k, 'v, 'cmp) container
      with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
      with type 'k key := 'k
      with type 'v value := 'v
  end

  (** {1 Comparators} *)

  val hash_fold: ('v -> Hash.State.t -> Hash.State.t) -> ('k, 'v, 'cmp) t
    -> Hash.State.t -> Hash.State.t
  (** [hash_fold hash_fold_v t state] incorporates the hash of [t] into [state]
      and returns the resulting state.  Mapping are stably hash-folded into the
      resulting state via the type's comparator [hash_fold] function for keys,
      and [hash_fold_v] for values. *)

  val cmper_m: ('k, 'v, 'cmp) t -> ('k, 'cmp) cmper
  (** [cmper_m t] returns a first class module that can be used to build other
      sets or maps with compatible comparison. *)

  val cmper: ('k, 'v, 'cmp) t -> ('k, 'cmp) Cmper.t
  (** [cmper t] returns the comparator associated with the map [t]. *)

  (** {1 Creation} *)

  val empty: ('k, 'cmp) cmper -> ('k, 'v, 'cmp) t
  (** [empty cmper] creates an empty map associated with [cmper], which is a
      first-class module encapsulating a comparator. *)

  val singleton: ('k, 'cmp) cmper -> k:'k -> v:'v -> ('k, 'v, 'cmp) t
  (** [singleton cmper ~k ~v] creates a map associated with [cmper] that maps
      [k] to [v]. *)

  val of_alist: ('k, 'cmp) cmper -> ('k * 'v) list -> ('k, 'v, 'cmp) t
  (** [of_alist cmper kvpairs] creates a map associated with [cmper] that
      contains the key-value mappings represented by [kvpairs].  Halts on
      duplicate keys. *)

  (** {1 Length} *)

  val length: ('k, 'v, 'cmp) t -> usize
  (** [length t] returns the number of mappings in [t].  O(1) time complexity.
  *)

  val is_empty: ('k, 'v, 'cmp) t -> bool
  (** [is_empty t] returns [true] if [t] has no mappings; [false] otherwise.
      O(1) time complexity. *)

  (** {1 Mapping operations} *)

  val mem: 'k -> ('k, 'v, 'cmp) t -> bool
  (** [mem k t] returns [true] if [k] is a key in [t]; [false] otherwise.  O(lg
      n) time complexity if ordered, O(1) time complexity if unordered. *)

  val get: 'k -> ('k, 'v, 'cmp) t -> 'v option
  (** [get k t] returns the value [Some v] associated with [k] if [k] is a key
      in [t], [None] otherwise.  O(lg n) time complexity if ordered, O(1) time
      complexity if unordered. *)

  val get_hlt: 'k -> ('k, 'v, 'cmp) t -> 'v
  (** [get_hlt k t] returns the value [v] associated with [k] if [k] is a key in
      [t], halts otherwise.  O(lg n) time complexity if ordered, O(1) time
      complexity if unordered. *)

  val choose: ('k, 'v, 'cmp) t -> ('k * 'v) option
  (** [choose t] returns an arbitrary key-value mapping in [t] if the map is
      non-empty, [None] otherwise. *)

  val choose_hlt: ('k, 'v, 'cmp) t -> ('k * 'v)
  (** [choose_hlt t] returns an arbitrary key-value mapping in [t] if the map is
      non-empty, halts otherwise. *)

  val insert: k:'k -> v:'v -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [insert ~k ~v t] returns an incremental derivative of [t] with [k] bound
      to [v] if [k] is not a key in [t], [t] otherwise.  O(lg n) time complexity
      if ordered, O(1) time complexity if unordered. *)

  val insert_hlt: k:'k -> v:'v -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [insert_hlt ~k ~v t] returns an incremental derivative of [t] with [k]
      bound to [v] if [k] is not a key in [t], halts otherwise.  O(lg n) time
      complexity if ordered, O(1) time complexity if unordered. *)

  val upsert: k:'k -> v:'v -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [upsert ~k ~v t] returns an incremental derivative of [t] with [k] bound
      to [v], regardless of whether [k] is a key in [t].  O(lg n) time
      complexity if ordered, O(1) time complexity if unordered. *)

  val update: k:'k -> v:'v -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [update ~k ~v t] returns an incremental derivative of [t] with [k] bound
      to [v] if [k] is a key in [t], [t] otherwise.  O(lg n) time complexity if
      ordered, O(1) time complexity if unordered. *)

  val update_hlt: k:'k -> v:'v -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [update_hlt ~k ~v t] returns an incremental derivative of [t] with [k]
      bound to [v] if [k] is a key in [t], halts otherwise.  O(lg n) time
      complexity if ordered, O(1) time complexity if unordered. *)

  val remove: 'k -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [remove k t] returns an incremental derivative of [t] with no binding for
      [k] if [k] is a key in [t], [t] otherwise.  O(lg n) time complexity if
      ordered, O(1) time complexity if unordered. *)

  val remove_hlt: 'k -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [remove_hlt k t] returns an incremental derivative of [t] with no binding
      for [k] if [k] is a key in [t], halts otherwise.  O(lg n) time complexity
      if ordered, O(1) time complexity if unordered. *)

  val amend: 'k -> f:('v option -> 'v option) -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
  (** [amend k ~f t] returns an incremental derivative of [t] that is equivalent
      to [t] in all mappings except possibly for key [k], as determined by the
      result of [~f v_opt], where [v_opt = Some v] indicates [k] is associated
      with [v] in [t], and [v_opt = None] indicates [k] is not a key in [t].
      The result contains a mapping from [k] to [v'] if [~f v_opt] returns [Some
      v']; the result contains no mapping for [k] if [~f v_opt] returns [None].
      O(lg n) time complexity if ordered, O(1) time complexity if unordered. *)

  (** {1 Map operations} *)

  val equal: ('v -> 'v -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** [equal veq t0 t1] returns [true] if [t0] and [t1] contain identical key
      sets and identical key-value mappings as determined by the key comparator
      and the [veq] value comparison function, [false] otherwise.  O(n) time
      complexity. *)

  val subset: ('v -> 'v -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** [subset veq t0 t1] returns [true] if all key-value mappings in [t1] are
      also in [t0], as determined by the key comparator and the [veq] value
      comparison function, [false] otherwise.  O(n) time complexity. *)

  val disjoint: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** [disjoint t0 t1] returns [true] if [t0] and [t1] contain disjoint key sets
      as determined by the key comparator, [false] otherwise.  O(n) time
      complexity. *)

  val union: f:('k -> 'v -> 'v -> 'v) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
  (** [union ~f t0 t1] creates a map that is the union of [t0] and [t1]; that
      is, a map that contains mappings for all keys present in [t0] or [t1],
      where the values for mappings in both [t0] and [t1] are remapped via [~f].
      O(m lg (n/m + 1)) time complexity if ordered, where m and n are the input
      map lengths and m <= n; Θ(m+n) time complexity if unordered. *)

  val inter: f:('k -> 'v -> 'v -> 'v) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
  (** [inter ~f t0 t1] creates a map that is the intersection of [t0] and [t1];
      that is, a map that contains mappings for all keys present in both [t0]
      and [t1], where the values are remapped via [~f].  O(m lg (n/m + 1)) time
      complexity if ordered, where m and n are the input map lengths and m <= n;
      Θ(m+n) time complexity if unordered. *)

  val diff: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [diff t0 t1] creates a map that is the difference of t0 relative to t1;
      that is, a map that contains all mappings present in [t0] but not present
      in [t1].  O(m lg (n/m + 1)) time complexity if ordered, where m and n are
      the input map lengths and m <= n; Θ(m+n) time complexity if unordered. *)

  (** {1 Folding, mapping, filtering, and reducing} *)

  val fold_until: init:'accum -> f:('accum -> ('k * 'v) -> 'accum * bool)
    -> ('k, 'v, 'cmp) t -> 'accum
  (** [fold_until ~init ~f t] folds [t] from left to right if ordered, or
      arbitrarily if unordered, using [init] as the initial accumulator value,
      continuing until [f] returns [accum, true], or until folding is complete
      if [f] always returns [accum, false]. *)

  val fold: init:'accum -> f:('accum -> ('k * 'v) -> 'accum) -> ('k, 'v, 'cmp) t
    -> 'accum
  (** [fold ~init ~f t] folds [t] from left to right if ordered, or arbitrarily
      if unordered, using [init] as the initial accumulator value. *)

  val iter: f:(('k * 'v) -> unit) -> ('k, 'v, 'cmp) t -> unit
  (** [iter ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t]. *)

  val count: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t -> usize
  (** [count ~f t] iterates over [t] and returns the number of times [f] returns
      [true]. *)

  val for_any: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t -> bool
  (** [for_any ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [true] if any invocation of [f] returns
      [true]. *)

  val for_all: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t -> bool
  (** [for_all ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [true] if all invocations of [f] return
      [true]. *)

  val find: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t -> ('k * 'v) option
  (** [find ~f t] iterates from left to right if ordered, or arbitrarily if
      unordered, over [t] and returns [Some (k, v)] for the first mapping which
      [f] returns [true], or [None] if [f] always returns [false]. *)

  val find_map: f:(('k * 'v) -> 'a option) -> ('k, 'v, 'cmp) t -> 'a option
  (** [find_map ~f t] iterates over [t] and returns [Some a] for a mapping which
      [f] returns [Some a], or [None] if [f] always returns [None]. *)

  val filter: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [filter ~f t] creates a map with contents filtered by [~f].  Only mappings
      for which the filter function returns [true] are incorporated into the
      result.  Θ(n) time complexity. *)

  val filter_map: f:(('k * 'v) -> 'v2 option) -> ('k, 'v, 'cmp) t
    -> ('k, 'v2, 'cmp) t
  (** [filter_map ~f t] creates a map with contents filtered and mapped by [~f].
      Only mappings for which the filter-map function returns [Some v2] are
      incorporated into the result.  Θ(n) time complexity. *)

  val partition_tf: f:(('k * 'v) -> bool) -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t
  (** [partition_tf ~f t] partitions [t] into two maps for which [~f] returns
      [true] vs [false].  Θ(n) time complexity. *)

  val partition_map: f:(('k * 'v) -> ('v2, 'v3) Either.t) -> ('k, 'v, 'cmp) t
    -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t
  (** [partition_map ~f t] partitions [t] into two maps for which [~f] returns
      [First v2] vs [Second v3].  Θ(n) time complexity. *)

  val kreduce: f:('k -> 'k -> 'k) -> ('k, 'v, 'cmp) t -> 'k option
  (** [kreduce ~f t] reduces [t] to a single key, or [None] if the map is empty.
      The reduction function is assumed to be associative; thus reduction order
      is unspecified. *)

  val kreduce_hlt: f:('k -> 'k -> 'k) -> ('k, 'v, 'cmp) t -> 'k
  (** [kreduce_hlt ~f t] reduces [t] to a single key, or halts if the map is
      empty.  The reduction function is assumed to be associative; thus
      reduction order is unspecified. *)

  val reduce: f:('v -> 'v -> 'v) -> ('k, 'v, 'cmp) t -> 'v option
  (** [reduce ~f t] reduces [t] to a single value, or [None] if the map is
      empty.  The reduction function is assumed to be associative; thus
      reduction order is unspecified. *)

  val reduce_hlt: f:('v -> 'v -> 'v) -> ('k, 'v, 'cmp) t -> 'v
  (** [reduce_hlt ~f t] reduces [t] to a single value, or halts if the map is
      empty.  The reduction function is assumed to be associative; thus
      reduction order is unspecified. *)

  include Seq_intf.S_poly3_fold2
    with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
     and type 'k key := 'k
     and type 'v value := 'v

  (** {1 Conversion} *)

  val to_alist: ('k, 'v, 'cmp) t -> ('k * 'v) list
  (** [to_alist t] folds [t] from right to left if ordered, or arbitrarily if
      unordered, as a {!type:('k * 'v) list}. *)

  include Container_array_intf.S_poly3_array
    with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
end

(** Ordered map. *)
module type S_ord = sig
  include S

  (** {1 Creation} *)

  val of_array: ('k, 'cmp) cmper -> ('k * 'v) array -> ('k, 'v, 'cmp) t
  (** [of_array cmper kvpairs] creates a map associated with [cmper] that
      contains [kvpairs].  Halts on duplicate keys. *)

  (** {1 Cursor} *)

  (** Cursor that supports arbitrary map member access.  [hd], [tl], [seek],
      [succ], and [pred] are O(lg n), but complete traversals via [succ] or
      [pred] are amortized O(1) per call.  [lget], [rget], [container], and
      [index] are O(1). *)
  module Cursor : sig
    type ('k, 'v, 'cmp) container = ('k, 'v, 'cmp) t
    type ('k, 'v, 'cmp) t

    include Cursor_intf.S_poly3
      with type ('k, 'v, 'cmp) container := ('k, 'v, 'cmp) container
      with type 'k key := 'k
      with type 'v value := 'v
      with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
  end

  (** {1 Mapping operations} *)

  val nth_opt: usize -> ('k, 'v, 'cmp) t -> ('k * 'v) option
  (** [nth i t] returns the nth map mapping (0-indexed), or [None] if [i] is out
      of bounds. *)

  val nth: usize -> ('k, 'v, 'cmp) t -> ('k * 'v)
  (** [nth i t] returns the nth map mapping (0-indexed), or halts if [i] is out
      of bounds. *)

  val psearch: 'k -> ('k, 'v, 'cmp) t -> (Cmp.t * usize) option
  (** [psearch k t] searches for [k] in [t], and falls back to the nearest
      present predecessor of [k] in the case of no match.
      @return {ul
        {- No predecessor: [Some (Cmp.Lt, 0)]}
        {- Leftmost match: [Some (Cmp.Eq, index)]}
        {- Predecessor: [Some (Cmp.Gt, index)]}
        {- Empty map: [None]}
      } *)

  val search: 'k -> ('k, 'v, 'cmp) t -> usize option
  (** [search k t] returns [(Some index)] if [k] is a key in [t]; [None]
      otherwise.  O(lg n) time complexity if ordered, O(1) time complexity if
      unordered. *)

  val nsearch: 'k -> ('k, 'v, 'cmp) t -> (Cmp.t * usize) option
  (** [nsearch k t] searches for [k] in [t], and falls back to the nearest
      present succesor of [k] in the case of no match.
      @return {ul
        {- Successor: [Some (Cmp.Lt, index)]}
        {- Match: [Some (Cmp.Eq, index)]}
        {- No successor: [Some (Cmp.Gt, (Usize.pred (length t)))]}
        {- Empty map: [None]}
      } *)

  (** {1 Map operations} *)

  val cmp: ('v -> 'v -> Cmp.t) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> Cmp.t
  (** [cmp vcmp t0 t1] compares [t0] and [t1].  O(m+n) time complexity, where m
      and n are the input map lengths. *)

  val split: 'k -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t
  (** [split k t] tripartitions [t] into mappings with keys \{<,=,>\} [k],
      respectively. *)

  (** {1 Folding, mapping, and filtering} *)

  val fold_right_until: init:'accum -> f:(('k * 'v) -> 'accum -> 'accum * bool)
    -> ('k, 'v, 'cmp) t -> 'accum
  (** [fold_right_until ~init ~f t] folds [t] from right to left, using [init]
      as the initial accumulator value, continuing until [f] returns [accum,
      true], or until folding is complete if [f] always returns [accum, false].
  *)

  val foldi_until: init:'accum -> f:(usize -> 'accum -> ('k * 'v)
    -> 'accum * bool) -> ('k, 'v, 'cmp) t -> 'accum
  (** [foldi_until ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value, continuing until [f]
      returns [accum, true], or until folding is complete if [f] always returns
      [accum, false]. *)

  val fold_right: init:'accum -> f:(('k * 'v) -> 'accum -> 'accum)
    -> ('k, 'v, 'cmp) t -> 'accum
  (** [fold_right ~init ~f t] folds [t] from left to right, using [init] as the
      initial accumulator value. *)

  val foldi: init:'accum -> f:(usize -> 'accum -> ('k * 'v) -> 'accum)
    -> ('k, 'v, 'cmp) t -> 'accum
  (** [foldi ~init ~f t] folds [t] with index provided to [f] from left to
      right, using [init] as the initial accumulator value. *)

  val iteri: f:(usize -> ('k * 'v) -> unit) -> ('k, 'v, 'cmp) t -> unit
  (** [iter ~f t] iterates with index provided from left to right over [t]. *)

  val findi: f:(usize -> ('k * 'v) -> bool) -> ('k, 'v, 'cmp) t
    -> ('k * 'v) option
  (** [findi ~f t] iterates from left to right over [t] with index provided to
      [f] and returns [Some (k, v)] for a mapping which [f] returns [true], or
      [None] if [f] always returns [false]. *)

  val findi_map: f:(usize -> ('k * 'v) -> 'a option) -> ('k, 'v, 'cmp) t
    -> 'a option
  (** [findi_map ~f t] iterates from left to right over [t] with index provided
      to [f] and returns [Some a] for a mapping which [f] returns [Some a], or
      [None] if [f] always returns [None]. *)

  val filteri: f:(usize -> ('k * 'v) -> bool) -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
  (** [filteri ~f t] creates a map with contents filtered by [~f].  Only
      mappings for which the filter function returns [true] are incorporated
      into the result.  Θ(n) time complexity. *)

  val filteri_map: f:(usize -> ('k * 'v) -> 'v2 option) -> ('k, 'v, 'cmp) t
    -> ('k, 'v2, 'cmp) t
  (** [filteri_map ~f t] creates a map with contents filtered and mapped by
      [~f].  Only mappings for which the filter-map function returns [Some v2]
      are incorporated into the result.  Θ(n) time complexity. *)

  val partitioni_tf: f:(usize -> ('k * 'v) -> bool) -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t
  (** [partitioni_tf ~f t] partitions [t] into two maps for which [~f] returns
      [true] vs [false].  Θ(n) time complexity. *)

  val partitioni_map: f:(usize -> ('k * 'v) -> ('v2, 'v3) Either.t)
    -> ('k, 'v, 'cmp) t -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t
  (** [partitioni_map ~f t] partitions [t] into two maps for which [~f] returns
      [First v2] vs [Second v3].  Θ(n) time complexity. *)

  val min_elm: cmp:(('k * 'v) -> ('k * 'v) -> Cmp.t) -> ('k, 'v, 'cmp) t
    -> ('k * 'v) option
  (** [min_elm ~f t] iterates from left to right over [t] and returns [Some (k,
      v)] for the leftmost element which always compares as [Cmp.Lt] or
      [Cmp.Eq], or [None] if [t] is empty. *)

  val max_elm: cmp:(('k * 'v) -> ('k * 'v) -> Cmp.t) -> ('k, 'v, 'cmp) t
    -> ('k * 'v) option
  (** [max_elm ~f t] iterates from left to right over [t] and returns [Some (k,
      v)] for the leftmost element which always compares as [Cmp.Eq] or
      [Cmp.Gt], or [None] if [t] is empty. *)

  (** {1 Conversion} *)

  val to_alist_rev: ('k, 'v, 'cmp) t -> ('k * 'v) list
  (** [to_alist_rev t] folds [t] from left to right as a {!type:('k * 'v) list}.
  *)
end
