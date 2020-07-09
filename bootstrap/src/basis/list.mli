(** Singly linked immutable {!type:list}.

    The {!type:list} type is a basic container that is appropriate for use in
    many sequential algorithms.  A {!type:list} is either empty, or it has a
    head element followed by a tail {!type:list}.
*)

open Rudiments

(** {1 Type and derivations} *)

type 'a t = 'a list

include Formattable_intf.S_poly with type 'a t := 'a t

(** {1 Container} *)

include Container_common_intf.S_poly_fold with type 'a t := 'a t

(** {1 Cursor} *)

(** Cursor that supports arbitrary list element access.  [tl] and [seek] are
    O(n); all other operations are O(1). *)
module Cursor : sig
  type 'a container = 'a t
  type 'a t

  include Cursor_intf.S_poly with type 'a container := 'a container
                              and type 'a elm := 'a
                              and type 'a t := 'a t
end

(** {1 Comparison} *)

val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
  -> Hash.State.t
(** [hash_fold hash_fold_a t state] incorporates the hash of [t] into [state]
    and returns the resulting state.  List elements are sequentially hash-folded
    into the resulting state via [hash_fold_a]. *)

val cmp: ('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> Cmp.t
(** Compare two lists using the element comparison function.  The list lengths
    may differ. *)

val cmp_length: 'a t -> 'a t -> Cmp.t
(** Compare the lengths of two lists. *)

val cmp_length_with: 'a t -> usize -> Cmp.t
(** Compare the length of a list to a given limit.  List traversal terminates
    after no more than (limit + 1) elements. *)

(** {1 Creation} *)

val init: usize -> f:(usize -> 'a) -> 'a t
(** Initialize list.  [init len ~f:(fun i -> ...)] initializes a list of given
    length, where [f] provides the value for each element at given index. *)

(** {1 Length} *)

val length: 'a t -> usize
(** Return list length. *)

val is_empty: 'a t -> bool
(** Return [true] if list length is 0; [false] otherwise. *)

(** {1 Element access} *)

val hd: 'a t -> 'a
(** Return head (first) list element, or halt if list is empty. *)

val tl: 'a t -> 'a t
(** Return tail (all elements except head), or halt if list is empty. *)

val nth_opt: usize -> 'a t -> 'a option
(** [nth i t] returns the nth list element (0-indexed), or [None] if [i] is out
    of bounds. *)

val nth: usize -> 'a t -> 'a
(** [nth i t] returns the nth list element (0-indexed), or halts if [i] is out
    of bounds. *)

(** {1 Combining, grouping, and partitioning} *)

val push: 'a -> 'a t -> 'a t
(* Language syntax does not allow ( :: ) to be redefined.
 * val ( :: ): 'a -> 'a t -> 'a t *)
(** Push element onto list and return resulting list.  Equivalent:
    - [push t elm]
    - [elm :: t]
*)

val pop: 'a t -> 'a * 'a t
(** Pop head element off list and return the decomposed element and remainder
    list.  Halt if the input list is empty. *)

val concat: 'a t -> 'a t -> 'a t
val ( @ ): 'a t -> 'a t -> 'a t
(** Concatenate two lists.  Not tail-recursive.  Equivalent:
    - [concat t0 t1]
    - [t0 @ t1]
*)

val join: ?sep:'a t -> 'a t t -> 'a t
(** Concatenate a list of lists.  Not tail-recursive. *)

val join_unordered: ?sep:'a t -> 'a t t -> 'a t
(** Concatenate a list of lists, with unspecified resulting order. *)

val rev_concat: 'a t -> 'a t -> 'a t
(** Equivalent to [concat (rev t0) t1], except that implementation is
    tail-recursive. *)

val concat_unordered: 'a t -> 'a t -> 'a t
(** Concatenate two lists, with unspecified resulting element order. *)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** Create a list with the paired elements of two lists.  Halt if list lengths
    differ. *)

val unzip: ('a * 'b) t -> 'a t * 'b t
(** Create two lists with the unpaired elements of the input pair list. *)

val split: usize -> 'a t -> 'a t * 'a t
(** Split the list with elements [\[0..len)] into lists with elements [\[0..n)]
    and [\[n..len)].  Halt if list contains fewer than [n] elements.  Not
    tail-recursive. *)

val rev_split: usize -> 'a t -> 'a t * 'a t
(** Split the list with elements [\[0..len)] into lists with elements [(n..0\]]
    and [\[n..len)].  Halt if list contains fewer than [n] elements. *)

val split_until: f:('a -> bool) -> 'a t -> 'a t * 'a t
(** Split the list such that [~f] returns false for all the elements before the
    split.  Not tail-recursive. *)

val rev_split_until: f:('a -> bool) -> 'a t -> 'a t * 'a t
(** Split the list such that [~f] returns false for all the elements before the
    split, and reverse the elements to the left of the split relative to the
    input. *)

val take: usize -> 'a t -> 'a t
(** [take i t] is equivalent to [fst (split t i)]. *)

val rev_take: usize -> 'a t -> 'a t
(** [rev_take i t] is equivalent to [fst (rev_split t i)]. *)

val take_until: f:('a -> bool) -> 'a t -> 'a t
(** [take_until ~f t] is equivalent to [fst (split_until t ~f)]. *)

val rev_take_until: f:('a -> bool) -> 'a t -> 'a t
(** [rev_take_until ~f t] is equivalent to [fst (rev_split_until t ~f)]. *)

val drop: usize -> 'a t -> 'a t
(** [drop i t] is equivalent to [snd (rev_split t i)]. *)

val drop_until: f:('a -> bool) -> 'a t -> 'a t
(** [drop_until ~f t] is equivalent to [snd (rev_split_until t ~f)]. *)

val partition_tf: f:('a -> bool) -> 'a t -> 'a t * 'a t
(** Partition the elements for which [~f] returns [true] vs [false] into two
    lists.  Not tail-recursive. *)

val rev_partition_tf: f:('a -> bool) -> 'a t -> 'a t * 'a t
(** Partition the elements for which [~f] returns [true] vs [false] into two
    lists, and reverse elements relative to the input. *)

val group: break:('a -> 'a -> bool) -> 'a t -> 'a t t
(** Break input list into a list of lists, where each break is determined by
    [~break] returning [true] for adjacent elements.  Not tail-recursive. *)

val groupi: break:(usize -> 'a -> 'a -> bool) -> 'a t -> 'a t t
(** Break input list into a list of lists, where each break is determined by
    [~break] returning [true] for adjacent elements, the second of which is at
    the given index.  Not tail-recursive. *)

val rev_group: break:('a -> 'a -> bool) -> 'a t -> 'a t t
(** [rev_group ~break t] is equivalent to [fold (group t ~break) ~init:[]
    ~f:(fun gs g -> (rev g) :: gs)], except that implementation is
    tail-recursive. *)

val rev_groupi: break:(usize -> 'a -> 'a -> bool) -> 'a t -> 'a t t
(** [rev_groupi ~break t] is equivalent to [fold (groupi t ~break) ~init:[]
    ~f:(fun gs g -> (rev g) :: gs)], except that implementation is
    tail-recursive. *)

(** {1 Re-ordering, mapping, reducing, and filtering} *)

val rev: 'a t -> 'a t
(** Create a list with elements reversed relative to the input list. *)

val reduce: f:('a -> 'a -> 'a) -> 'a t -> 'a option
(** Reduce the list to a single value, or return [None] if the list is empty.
    The reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val reduce_hlt: f:('a -> 'a -> 'a) -> 'a t -> 'a
(** Reduce the list to a single value, or halt if the list is empty.  The
    reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val is_sorted: ?strict:bool -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> bool
(** Return true if list is sorted (strictly if [?strict] is [true]; default is
    non-strict) according to the comparison function, [false] otherwise. *)

val sort: ?length:usize -> ?stable:bool -> cmp:('a -> 'a -> Cmp.t) -> 'a t
  -> 'a t
(** Create a list with sorted contents of the input list according to the
    comparison function.  If specified, [?length] must equal [(length list)].
    Preserve order of equivalent elements if [?stable] is [true]. *)

val dedup: ?length:usize -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t
(** Sort list stably and remove subsequent duplicates in forward order.  If
    specified, [?length] must equal [(length list)].  Not tail-recursive. *)

val rev_dedup: ?length:usize -> cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t
(** Sort list stably, remove subsequent duplicates in forward order, and reverse
    elements relative to the input.  If specified, [?length] must equal [(length
    list)].  [rev_dedup t ~cmp] is equivalent to [rev (dedup t ~cmp)]. *)

val dedup_sorted: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t
(** Remove sebsequent adjacent duplicates in forward order. *)

val rev_dedup_sorted: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t
(** Remove sebsequent adjacent duplicates in forward order and reverse elements
    relative to the input. *)

val merge: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> 'a t
(** Merge sorted input lists into a sorted output list.  For [(merge ~cmp t0
    t1)], equal elements from [t0] precede those from [t1].  Not tail-recursive.
*)

val rev_merge: cmp:('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> 'a t
(** Merge sorted input lists into a reverse-sorted output list.  For [(rev_merge
    ~cmp t0 t1)], equal elements from [t1] precede those from [t0].  [rev_merge
    ~cmp t0 t1] is equivalent to [rev (merge ~cmp t0 t1)]. *)

val map: f:('a -> 'b) -> 'a t -> 'b t
(** Create a list with elements mapped from the input list, according to the
    element mapping function.  Not tail-recursive. *)

val mapi: f:(usize -> 'a -> 'b) -> 'a t -> 'b t
(** Create a list with elements mapped from the input list, according to the
    indexed element mapping function.  Not tail-recursive. *)

val rev_map: f:('a -> 'b) -> 'a t -> 'b t
(** Create a reversed list with elements mapped from the input list, according
    to the element mapping function. *)

val rev_mapi: f:(usize -> 'a -> 'b) -> 'a t -> 'b t
(** Create a reversed list with elements mapped from the input list, according
    to the indexed element mapping function. *)

val rev_map_concat: f:('a -> 'b) -> 'a t -> 'b t -> 'b t
(** For [rev_map_concat ~f t0 t1], concatenate the reversed mapping of [t0] with
    [t1].  Equivalent to [rev_concat (map ~f t0) t1], except that the
    implementation is tail-recursive. *)

val fold_map: init:'accum -> f:('accum -> 'a -> 'accum * 'b) -> 'a t
  -> 'accum * 'b t
(** Create a list and accumulated result with elements mapped from the input
    list, according to the folding mapping function.  Not tail-recursive. *)

val foldi_map: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum * 'b) -> 'a t
  -> 'accum * 'b t
(** Create a list and accumulated result with elements mapped from the input
    list, according to the indexed folding mapping function.  Not
    tail-recursive. *)

val rev_fold_map: init:'accum -> f:('accum -> 'a -> 'accum * 'b) -> 'a t
  -> 'accum * 'b t
(** Create a reversed list and accumulated result with elements mapped from the
    input list, according to the folding mapping function. *)

val rev_foldi_map: init:'accum -> f:(usize -> 'accum -> 'a -> 'accum * 'b)
  -> 'a t -> 'accum * 'b t
(** Create a reversed list and accumulated result with elements mapped from the
    input list, according to the indexed folding mapping function. *)

val filter: f:('a -> bool) -> 'a t -> 'a t
(** Create a list with contents filtered by the given function.  Only elements
    for which the filter function returns true are incorporated in the result.
    Not tail-recursive. *)

val filteri: f:(usize -> 'a -> bool) -> 'a t -> 'a t
(** Create a list with contents filtered by the given function.  Only elements
    for which the indexed filter function returns true are incorporated in the
    result.  Not tail-recursive. *)

val rev_filter: f:('a -> bool) -> 'a t -> 'a t
(** Create a reversed list with contents filtered by the given function.  Only
    elements for which the filter function returns true are incorporated in the
    result. *)

val rev_filteri: f:(usize -> 'a -> bool) -> 'a t -> 'a t
(** Create a reversed list with contents filtered by the given function.  Only
    elements for which the indexed filter function returns true are incorporated
    in the result. *)

val fold2_until: init:'accum -> f:('accum -> 'a -> 'b -> 'accum * bool) -> 'a t
  -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two lists, calling
    the element folding function in increasing index order, and terminate
    folding early if the folding function returns true. *)

val foldi2_until: init:'accum
  -> f:(usize -> 'accum -> 'a -> 'b -> 'accum * bool) -> 'a t -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two lists, calling
    the indexed element folding function in increasing index order, and
    terminate folding early if the folding function returns true. *)

val fold2: init:'accum -> f:('accum -> 'a -> 'b -> 'accum) -> 'a t -> 'b t
  -> 'accum
(** Create an accumulated result for the paired elements of two lists, calling
    the element folding function in increasing index order. *)

val foldi2: init:'accum -> f:(usize -> 'accum -> 'a -> 'b -> 'accum) -> 'a t
  -> 'b t -> 'accum
(** Create an accumulated result for the paired elements of two lists, calling
    the indexed element folding function in increasing index order. *)

val iter2: f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate over the paired elements of two lists, calling the element visiting
    function in increasing index order. *)

val iteri2: f:(usize -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate over the paired elements of two lists, calling the indexed element
    visiting function in increasing index order. *)

val map2: f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create a list with elements mapped by the element mapping function from the
    paired elements of two input lists. *)

val mapi2: f:(usize -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create a list with elements mapped by the indexed element mapping function
    from the paired elements of two input lists. *)

val rev_map2: f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create a list with elements mapped by the element mapping function from the
    paired elements of two input lists, and reverse the elements relative to the
    input lists' associated order. *)

val rev_mapi2: f:(usize -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Create a list with elements mapped by the indexed element mapping function
    from the paired elements of two input lists, and reverse the elements
    relative to the input lists' associated order. *)

val fold2_map: init:'accum -> f:('accum -> 'a -> 'b -> 'accum * 'c) -> 'a t
  -> 'b t -> 'accum * 'c t
(** Create a list and accumulated result based on the paired elements of two
    lists, calling the element folding/mapping function in increasing index
    order.  Not tail-recursive. *)

val foldi2_map: init:'accum -> f:(usize -> 'accum -> 'a -> 'b -> 'accum * 'c)
  -> 'a t -> 'b t -> 'accum * 'c t
(** Create a list and accumulated result based on the paired elements of two
    lists, calling the indexed element folding/mapping function in increasing
    index order.  Not tail-recursive. *)

val rev_fold2_map: init:'accum -> f:('accum -> 'a -> 'b -> 'accum * 'c) -> 'a t
  -> 'b t -> 'accum * 'c t
(** Create a reversed list and accumulated result based on the paired elements
    of two lists, calling the element folding/mapping function in increasing
    index order. *)

val rev_foldi2_map: init:'accum
  -> f:(usize -> 'accum -> 'a -> 'b -> 'accum * 'c) -> 'a t -> 'b t
  -> 'accum * 'c t
(** Create a reversed list and accumulated result based on the paired elements
    of two lists, calling the indexed element folding/mapping function in
    increasing index order. *)

(** {1 Associative maps} *)

(** Treat a list of [(key * value)] pairs as a map, with the caveat that
    subsequent duplicate keys are shadowed. *)
module Assoc : sig
  type nonrec ('a, 'b) t = ('a * 'b) t

  val add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  (** Associate value with key in list, shadowing existing association for
      equivalent key, if any. *)

  val find: 'a -> cmp:('a -> 'a -> Cmp.t) -> ('a, 'b) t -> 'b option
  (** Find value associated with key, if present. *)

  val find_hlt: 'a -> cmp:('a -> 'a -> Cmp.t) -> ('a, 'b) t -> 'b
  (** Find value associated with key, or halt if not present. *)

  val mem: 'a -> cmp:('a -> 'a -> Cmp.t) -> ('a, 'b) t -> bool
  (** Return whether key is present. *)

  val remove: 'a -> cmp:('a -> 'a -> Cmp.t) -> ('a, 'b) t -> ('a, 'b) t
  (** Remove association, if present.  Not tail-recursive. *)

  val remove_hlt: 'a -> cmp:('a -> 'a -> Cmp.t) -> ('a, 'b) t -> ('a, 'b) t
  (** Remove association, or halt if not present.  Not tail-recursive. *)

  val map: f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Create a new associative list with values mapped from the input list via
      the map function.  Not tail-recursive. *)

  val inverse: ('a, 'b) t -> ('b, 'a) t
  (** Create a new associative list with the [(key * value)] pairs inverted
      relative to the input list.  Output order is unspecified; thus shadowing
      is arbitrary. *)
end
