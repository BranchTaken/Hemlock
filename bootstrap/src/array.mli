open Rudiments

type 'a t = 'a array [@@deriving sexp]
include Container_common_intf.S_poly_fold with type 'a t := 'a t

(** Cursor that supports arbitrary array element access.  All operations are
    O(1). *)
module Cursor : sig
  type 'a container = 'a t
  type 'a t

  include Cursor_intf.S_poly with type 'a container := 'a container
                              and type 'a elm := 'a
                              and type 'a t := 'a t
end

val cmp: ('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> Cmp.t
(** Compare two arrays given the element comparison function.  The array lengths
    may differ. *)

module Seq : sig
  type 'a outer = 'a t
  module type S_poly = sig
    type 'a t
    type 'a elm
    val to_array: 'a t -> 'a elm outer
  end
  module type S_mono = sig
    type t
    type elm
    val to_array: t -> elm outer
  end

  (** Efficiently convert a generic sequence with known length to an array. *)
  module Make_poly (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm

  (** Efficiently convert a reversed generic sequence with known length to an
      array. *)
  module Make_poly_rev (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm

  (** Efficiently convert a sequence of fixed element type with known length to
      an array. *)
  module Make_mono (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                       and type elm := T.elm

  (** Efficiently convert a reversed sequence of fixed element type with known
      length to an array. *)
  module Make_mono_rev (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                           and type elm := T.elm
end

val init: uint -> f:(uint -> 'a) -> 'a t
(** Initialize array.  [init len ~f:(fun i -> ...)] initializes an array of
    given length, where [f] provides the value for each element at given index.
*)

val of_list: ?length:uint -> 'a list -> 'a t
(** Initialize array using contents of list.  If specified, [?length] must equal
    [(List.length list)]. *)

val of_list_rev: ?length:uint -> 'a list -> 'a t
(** Initialize array using reversed contents of list.  If specified, [?length]
    must equal [(List.length list)]. *)

(* In Container_common_intf.S_poly_fold:
val to_list: 'a t -> 'a list
val to_list_rev: 'a t -> 'a list
*)

val length: 'a t -> uint
(** Return array length. *)

val is_empty: 'a t -> bool
(** Return [true] if array length is 0; [false] otherwise. *)

val get: 'a t -> uint -> 'a
(** Get array element.  [get t i] returns the element at index [i]. *)

val set_inplace: 'a t -> uint -> 'a -> unit
(** Set array element in place (mutate).  [set_inplace t i elm] sets the element
    at index [i] to [elm]. *)

val set: 'a t -> uint -> 'a -> 'a t
(** Create a new array based on input array, differing in one element.
    [set t i elm] creates an array equal to [t], except that element [i] is
    initialized to [elm]. *)

val copy: 'a t -> 'a t
(** Create a copy of an array. *)

val pare: 'a t -> base:uint -> past:uint -> 'a t
(** Create an array with contents initialized to equal the \[[base]..[past])
    input subarray. *)

val join: ?sep:'a t -> 'a t list -> 'a t
(** Concatenate a list of arrays, with optional separator. *)

val concat: 'a t -> 'a t -> 'a t
(** Concatenate two arrays. *)

val append: 'a t -> 'a -> 'a t
(** Create an array that is the concatenation of the input array and the input
    element. *)

val prepend: 'a -> 'a t -> 'a t
(** Create an array that is the concatenation of the input element and the input
    array. *)

val insert: 'a t -> uint -> 'a -> 'a t
(** Create an array that is the concatenation of the bipartition of the input
    array at specified index, with the input element interposed. *)

val remove: 'a t -> uint -> 'a t
(** Create an array that is the concatenation of the first and third components
    of the tripartition about the element at specified index. *)

val reduce: 'a t -> f:('a -> 'a -> 'a) -> 'a option
(** Reduce the array to a single value, or return [None] if the array is empty.
    The reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val reduce_hlt: 'a t -> f:('a -> 'a -> 'a) -> 'a
(** Reduce the array to a single value, or halt if the array is empty.  The
    reduction function is assumed to be associative; thus reduction order is
    unspecified. *)

val swap_inplace: 'a t -> uint -> uint -> unit
(** Swap elements at given indices in place (mutate). *)

val swap: 'a t -> uint -> uint -> 'a t
(** Create an array based on the input array, but with elements at given indices
    swapped. *)

val rev_inplace: 'a t -> unit
(** Reverse array in place (mutate). *)

val rev: 'a t -> 'a t
(** Create an array with contents reversed relative to the input array. *)

val blit: 'a t -> uint -> 'a t -> uint -> uint -> unit
(** Set a range of elements in place (mutate).  [blit t0 i0 t1 i1 len] sets the
    elements of [t1] at [\[i1..i1+len)] to equal the elements of [t0] at
    [\[i0..i0+len)].  Overlapping ranges are supported: [t0] and [t1] may refer
    to the same or different arrays; [i0] and [i1] may have arbitrary
    relationship. *)

val is_sorted: ?strict:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> bool
(** Return true if array is sorted (strictly if [?strict] is [true]) according
    to the comparison function. *)

val sort_inplace: ?stable:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> unit
(** Sort the array in place (mutate) according to the comparison function.
    Preserve order of equivalent elements if [?stable] is [true]. *)

val sort: ?stable:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> 'a t
(** Create an array with sorted contents of the input array according to the
    comparison function.  Preserve order of equivalent elements if [?stable] is
    [true]. *)

val psearch: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> (Cmp.t * uint) option
(** Binary search for key in array, selecting the leftmost match, and falling
    back to the predecessor in the case of no match.
    @return {ul
      {- No predecessor: [Some (Cmp.Lt, 0)]}
      {- Leftmost match: [Some (Cmp.Eq, index)]}
      {- Predecessor: [Some (Cmp.Gt, index)]}
      {- Empty array: [None]}
    }
*)

val search: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> uint option
(** Binary search for key in array.  If key is found, return [(Some index)],
    otherwise return [None].  Note that if more than one element matches key, an
    arbitrary match is returned. *)

val nsearch: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> (Cmp.t * uint) option
(** Binary search for key in array, selecting the rightmost match, and falling
    back to the successor.
    @return {ul
      {- Successor: [Some (Cmp.Lt, index)]}
      {- Rightmost match: [Some (Cmp.Eq, index)]}
      {- No successor: [Some (Cmp.Gt, (Uint.pred (length t)))]}
      {- Empty array: [None]}
    }
*)

val map: 'a t -> f:('a -> 'b) -> 'b t
(** Create an array with elements mapped from the input array, according to the
    element mapping function. *)

val mapi: 'a t -> f:(uint -> 'a -> 'b) -> 'b t
(** Create an array with elements mapped from the input array, according to the
    indexed element mapping function. *)

val fold_map: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum * 'b)
  -> 'accum * 'b t
(** Create an array and accumulated result with elements mapped from the input
    array, according to the folding mapping function. *)

val foldi_map: 'a t -> init:'accum -> f:(uint -> 'accum -> 'a -> 'accum * 'b)
  -> 'accum * 'b t
(** Create an array and accumulated result with elements mapped from the input
    array, according to the indexed folding mapping function. *)

val filter: 'a t -> f:('a -> bool) -> 'a t
(** Create an array with contents filtered by the given function.  Only elements
    for which the filter function returns true are incorporated in the result.
*)

val filteri: 'a t -> f:(uint -> 'a -> bool) -> 'a t
(** Create an array with contents filtered by the given function.  Only elements
    for which the indexed filter function returns true are incorporated in the
    result. *)

val fold2_until: 'a t -> 'b t -> init:'accum
  -> f:('accum -> 'a -> 'b -> 'accum * bool) -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the element folding function in increasing index order, and terminate
    folding early if the folding function returns true. *)

val foldi2_until: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum * bool) -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the indexed element folding function in increasing index order, and
    terminate folding early if the folding function returns true. *)

val fold2: 'a t -> 'b t -> init:'accum -> f:('accum -> 'a -> 'b -> 'accum)
  -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the element folding function in increasing index order. *)

val foldi2: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum) -> 'accum
(** Create an accumulated result for the paired elements of two arrays, calling
    the indexed element folding function in increasing index order. *)

val iter2: 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
(** Iterate over the paired elements of two arrays, calling the element visiting
    function in increasing index order. *)

val iteri2: 'a t -> 'b t -> f:(uint -> 'a -> 'b -> unit) -> unit
(** Iterate over the paired elements of two arrays, calling the indexed element
    visiting function in increasing index order. *)

val map2: 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
(** Create an array with elements mapped by the element mapping function from
    the paired elements of two input arrays. *)

val mapi2: 'a t -> 'b t -> f:(uint -> 'a -> 'b -> 'c) -> 'c t
(** Create an array with elements mapped by the indexed element mapping function
    from the paired elements of two input arrays. *)

val fold2_map: 'a t -> 'b t -> init:'accum
  -> f:('accum -> 'a -> 'b -> 'accum * 'c) -> 'accum * 'c t
(** Create an array and accumulated result based on the paired elements of two
    arrays, calling the element folding/mapping function in increasing index
    order. *)

val foldi2_map: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum * 'c) -> 'accum * 'c t
(** Create an array and accumulated result based on the paired elements of two
    arrays, calling the indexed element folding/mapping function in increasing
    index order. *)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** Create an array with the paired elements of two arrays. *)

val unzip: ('a * 'b) t -> 'a t * 'b t
(** Create two arrays with the unpaired elements of the input pair array. *)

val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp pp_elm ppf t] prints a syntactic representation of [t] to the pretty
    printing formatter, [ppf], using the [pp_elm] printer for the elements.
    This function is intended for use with the [%a] format specifier to
    {!Format.printf}. *)
