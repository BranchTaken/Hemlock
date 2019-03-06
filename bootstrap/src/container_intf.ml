(* Partial Rudiments. *)
module Int = I63
type 'a array = 'a Array.t
type int = Int.t

(* Polymorphic container, e.g. ('a array). *)

module type I_poly = sig
  type 'a t
  type 'a elm
  module Cursor : sig
    include Cursor_intf.S_poly_iter with type 'a container := 'a t
                                     and type 'a elm := 'a elm
  end
end

module type S_poly_length = sig
  type 'a t
  type 'a elm
  val length: 'a t -> int
  val is_empty: 'a t -> bool
end

module type S_poly_fold = sig
  type 'a t
  type 'a elm
  val fold_until: 'a t -> init:'accum -> f:('accum -> 'a elm -> 'accum * bool)
    -> 'accum
  val fold_right_until: 'a t -> init:'accum
    -> f:('a elm -> 'accum -> 'accum * bool) -> 'accum
  val foldi_until: 'a t -> init:'accum
    -> f:(int -> 'accum -> 'a elm -> 'accum * bool) -> 'accum
  val fold: 'a t -> init:'accum -> f:('accum -> 'a elm -> 'accum) -> 'accum
  val fold_right: 'a t -> init:'accum -> f:('a elm -> 'accum -> 'accum)
    -> 'accum
  val foldi: 'a t -> init:'accum -> f:(int -> 'accum -> 'a elm -> 'accum)
    -> 'accum
  val iter: 'a t -> f:('a elm -> unit) -> unit
  val iteri: 'a t -> f:(int -> 'a elm -> unit) -> unit
  val count: 'a t -> f:('a elm -> bool) -> int
  val for_any: 'a t -> f:('a elm -> bool) -> bool
  val for_all: 'a t -> f:('a elm -> bool) -> bool
  val find: 'a t -> f:('a elm -> bool) -> 'a elm option
  val find_map: 'a t -> f:('a elm -> 'b option) -> 'b option
  val min_elm: 'a t -> cmp:('a elm -> 'a elm -> Cmp.t) -> 'a elm option
  val max_elm: 'a t -> cmp:('a elm -> 'a elm -> Cmp.t) -> 'a elm option
  val to_list: 'a t -> 'a elm list
  val to_list_rev: 'a t -> 'a elm list
end

module type I_poly_mem = sig
  include I_poly
  val cmp_elm: 'a elm -> 'a elm -> Cmp.t
end

module type S_poly_mem = sig
  type 'a t
  type 'a elm
  val mem: 'a t -> 'a elm -> bool
end

module type I_poly_array = sig
  include I_poly
  val length: 'a t -> int
end

module type S_poly_array = sig
  type 'a t
  type 'a elm
  val to_array: 'a t -> 'a elm array
end

module type S_poly = sig
  include S_poly_length
  include S_poly_fold with type 'a t := 'a t and type 'a elm := 'a elm
  include S_poly_mem with type 'a t := 'a t and type 'a elm := 'a elm
  include S_poly_array with type 'a t := 'a t and type 'a elm := 'a elm
end

(* Monomorphic, e.g. string. *)

module type I_mono = sig
  type t
  type elm
  module Cursor : sig
    include Cursor_intf.S_mono_iter with type container := t and type elm := elm
  end
end

module type S_mono_length = sig
  type t
  type elm
  val length: t -> int
  val is_empty: t -> bool
end

module type S_mono_fold = sig
  type t
  type elm
  val fold_until: t -> init:'accum -> f:('accum -> elm -> 'accum * bool)
    -> 'accum
  val fold_right_until: t -> init:'accum -> f:(elm -> 'accum -> 'accum * bool)
    -> 'accum
  val foldi_until: t -> init:'accum -> f:(int -> 'accum -> elm -> 'accum * bool)
    -> 'accum
  val fold: t -> init:'accum -> f:('accum -> elm -> 'accum) -> 'accum
  val fold_right: t -> init:'accum -> f:(elm -> 'accum -> 'accum) -> 'accum
  val foldi: t -> init:'accum -> f:(int -> 'accum -> elm -> 'accum) -> 'accum
  val iter: t -> f:(elm -> unit) -> unit
  val iteri: t -> f:(int -> elm -> unit) -> unit
  val count: t -> f:(elm -> bool) -> int
  val for_any: t -> f:(elm -> bool) -> bool
  val for_all: t -> f:(elm -> bool) -> bool
  val find: t -> f:(elm -> bool) -> elm option
  val find_map: t -> f:(elm -> 'a option) -> 'a option
  val min_elm: t -> cmp:(elm -> elm -> Cmp.t) -> elm option
  val max_elm: t -> cmp:(elm -> elm -> Cmp.t) -> elm option
  val to_list: t -> elm list
  val to_list_rev: t -> elm list
end

module type I_mono_mem = sig
  include I_mono
  val cmp_elm: elm -> elm -> Cmp.t
end

module type S_mono_mem = sig
  type t
  type elm
  val mem: t -> elm -> bool
end

module type I_mono_array = sig
  include I_mono
  val length: t -> int
end

module type S_mono_array = sig
  type t
  type elm
  val to_array: t -> elm array
end

module type S_mono = sig
  include S_mono_length
  include S_mono_fold with type t := t and type elm := elm
  include S_mono_mem with type t := t and type elm := elm
  include S_mono_array with type t := t and type elm := elm
end

