(* Partial Rudiments. *)
module Uint = U63
type uint = Uint.t

type 'a t = 'a array [@@deriving sexp]
type 'a elm
include Container_common_intf.S_poly_fold with type 'a t := 'a t

val cmp: ('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> Cmp.t

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
  module Make_poly (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm
  module Make_poly_rev (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm
  module Make_mono (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                       and type elm := T.elm
  module Make_mono_rev (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                           and type elm := T.elm
end

val init: uint -> f:(uint -> 'a) -> 'a t
val of_list: ?length:uint -> 'a list -> 'a t
val of_list_rev: ?length:uint -> 'a list -> 'a t
(* In Container_common_intf.S_poly_fold:
val to_list: 'a t -> 'a list
val to_list_rev: 'a t -> 'a list
*)

val length: 'a t -> uint
val is_empty: 'a t -> bool

val get: 'a t -> uint -> 'a

val set_inplace: 'a t -> uint -> 'a -> unit
val set: 'a t -> uint -> 'a -> 'a t

val copy: 'a t -> 'a t
val pare: 'a t -> base:uint -> past:uint -> 'a t
val concat_list: 'a t list -> 'a t
val concat: 'a t -> 'a t -> 'a t
val append: 'a t -> 'a -> 'a t
val prepend: 'a -> 'a t -> 'a t
val insert: 'a t -> uint -> 'a -> 'a t
val remove: 'a t -> uint -> 'a t

val reduce: 'a t -> f:('a -> 'a -> 'a) -> 'a option
val reduce_hlt: 'a t -> f:('a -> 'a -> 'a) -> 'a

val swap_inplace: 'a t -> uint -> uint -> unit
val swap: 'a t -> uint -> uint -> 'a t

val rev_inplace: 'a t -> unit
val rev: 'a t -> 'a t

val blit: 'a t -> uint -> 'a t -> uint -> uint -> unit

val is_sorted: ?strict:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> bool
val sort_inplace: ?stable:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> unit
val sort: ?stable:bool -> 'a t -> cmp:('a -> 'a -> Cmp.t) -> 'a t

val psearch: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> (Cmp.t * uint) option
(** Binary search for key in array, selecting the leftmost match, and falling
    back to the predecessor in the case of no match.
    - No predecessor: Some (Cmp.Lt, 0)
    - Leftmost match: Some (Cmp.Eq, index)
    - Predecessor: Some (Cmp.Gt, index)
    - Empty array: None
    *)

val search: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> uint option
(** Binary search for key in array.  If key is found, return (Some index),
    otherwise return None.  Note that if more than one element matches key, an
    arbitrary match is returned. *)

val nsearch: ?base:uint -> ?past:uint -> 'a t -> 'a -> cmp:('a -> 'a -> Cmp.t)
  -> (Cmp.t * uint) option
(** Binary search for key in array, selecting the rightmost match, and falling
    back to the successor.
    - Successor: Some (Cmp.Lt, index)
    - Rightmost match: Some (Cmp.Eq, index)
    - No successor: Some (Cmp.Gt, (Uint.pred (length t)))
    - Empty array: None
    *)

val map: 'a t -> f:('a -> 'b) -> 'b t
val mapi: 'a t -> f:(uint -> 'a -> 'b) -> 'b t

val fold_map: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum * 'b)
  -> 'accum * 'b t
val foldi_map: 'a t -> init:'accum -> f:(uint -> 'accum -> 'a -> 'accum * 'b)
  -> 'accum * 'b t

val filter: 'a t -> f:('a -> bool) -> 'a t
val filteri: 'a t -> f:(uint -> 'a -> bool) -> 'a t

val fold2_until: 'a t -> 'b t -> init:'accum
  -> f:('accum -> 'a -> 'b -> 'accum * bool) -> 'accum
val foldi2_until: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum * bool) -> 'accum

val fold2: 'a t -> 'b t -> init:'accum -> f:('accum -> 'a -> 'b -> 'accum)
  -> 'accum
val foldi2: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum) -> 'accum

val iter2: 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val iteri2: 'a t -> 'b t -> f:(uint -> 'a -> 'b -> unit) -> unit

val map2: 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val mapi2: 'a t -> 'b t -> f:(uint -> 'a -> 'b -> 'c) -> 'c t

val fold2_map: 'a t -> 'b t -> init:'accum
  -> f:('accum -> 'a -> 'b -> 'accum * 'c) -> 'accum * 'c t
val foldi2_map: 'a t -> 'b t -> init:'accum
  -> f:(uint -> 'accum -> 'a -> 'b -> 'accum * 'c) -> 'accum * 'c t

val zip: 'a t -> 'b t -> ('a * 'b) t
val unzip: ('a * 'b) t -> 'a t * 'b t
