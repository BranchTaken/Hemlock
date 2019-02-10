(* Partial Rudiments. *)
module Int = I63
type 'a array = 'a Array.t
type int = Int.t

module type I_mono = sig
  type t
  type elm
  module Cursor : sig
    include Cursor_intf.S_iter with type container := t and type elm := elm
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
  type t
  type elm
  module Cursor : sig
    include Cursor_intf.S_iter with type container := t and type elm := elm
  end
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
