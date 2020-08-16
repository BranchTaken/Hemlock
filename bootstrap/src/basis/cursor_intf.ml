(** Cursor interfaces. *)

open Rudiments0

(** Forward-only cursor iterator functor output signature for monomorphic types.
*)
module type S_mono_fwd_iter = sig
  type container
  (** Container type. *)

  type elm
  (** Element type. *)

  type t
  (** Cursor type. *)

  include Cmpable_intf.S_mono with type t := t

  val hd: container -> t
  (** Return head. *)

  val tl: container -> t
  (** Return tail. *)

  val succ: t -> t
  (** Return successor. *)

  val rget: t -> elm
  (** Return element immediately to right. *)

  val next: t -> elm * t
  (** [next t] is equivalent to [rget t, succ t], but potentially more
      efficient. *)
end

(** Cursor iterator functor output signature for monomorphic types, e.g.
    [string]. *)
module type S_mono_iter = sig
  include S_mono_fwd_iter

  val pred: t -> t
  (** Return predecessor. *)

  val lget: t -> elm
  (** Return element immediately to left. *)

  val prev: t -> elm * t
  (** [prev t] is equivalent to [lget t, pred t], but potentially more
      efficient. *)
end

(** Forward-only cursor functor output signature for monomorphic types, e.g.
    [Text.t]. *)
module type S_mono_fwd = sig
  include S_mono_fwd_iter

  val container: t -> container
  (** Return container associated with iterator. *)

  val index: t -> uns
  (** Return iterator index. *)

  val seek_fwd: uns -> t -> t
  (** [seek_fwd i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor functor output signature for monomorphic types, e.g. [string]. *)
module type S_mono = sig
  include S_mono_iter

  val container: t -> container
  (** Return container associated with iterator. *)

  val index: t -> uns
  (** Return iterator index. *)

  val seek: sint -> t -> t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [('a
    array)]. *)
module type S_poly_iter = sig
  type 'a container
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  type 'a t
  (** Cursor type. *)

  include Cmpable_intf.S_poly with type 'a t := 'a t

  val hd: 'a container -> 'a t
  (** Return head. *)

  val tl: 'a container -> 'a t
  (** Return tail. *)

  val succ: 'a t -> 'a t
  (** Return successor. *)

  val pred: 'a t -> 'a t
  (** Return predecessor. *)

  val lget: 'a t -> 'a elm
  (** Return element immediately to left. *)

  val rget: 'a t -> 'a elm
  (** Return element immediately to right. *)

  val prev: 'a t -> 'a elm * 'a t
  (** [prev t] is equivalent to [lget t, pred t], but potentially more
      efficient. *)

  val next: 'a t -> 'a elm * 'a t
  (** [next t] is equivalent to [rget t, succ t], but potentially more
      efficient. *)
end

(** Cursor functor output signature for polymorphic types, e.g. [('a array)]. *)
module type S_poly = sig
  include S_poly_iter

  val container: 'a t -> 'a container
  (** Return container associated with iterator. *)

  val index: 'a t -> uns
  (** Return iterator index. *)

  val seek: sint -> 'a t -> 'a t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [(('a,
    'cmp) Ordset)]. *)
module type S_poly2_iter = sig
  type ('a, 'cmp) container
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  type ('a, 'cmp) t
  (** Cursor type. *)

  include Cmpable_intf.S_poly2 with type ('a, 'cmp) t := ('a, 'cmp) t

  val hd: ('a, 'cmp) container -> ('a, 'cmp) t
  (** Return head. *)

  val tl: ('a, 'cmp) container -> ('a, 'cmp) t
  (** Return tail. *)

  val succ: ('a, 'cmp) t -> ('a, 'cmp) t
  (** Return successor. *)

  val pred: ('a, 'cmp) t -> ('a, 'cmp) t
  (** Return predecessor. *)

  val lget: ('a, 'cmp) t -> 'a elm
  (** Return element immediately to left. *)

  val rget: ('a, 'cmp) t -> 'a elm
  (** Return element immediately to right. *)

  val prev: ('a, 'cmp) t -> 'a elm * ('a, 'cmp) t
  (** [prev t] is equivalent to [lget t, pred t], but potentially more
      efficient. *)

  val next: ('a, 'cmp) t -> 'a elm * ('a, 'cmp) t
  (** [next t] is equivalent to [rget t, succ t], but potentially more
      efficient. *)
end

(** Cursor functor output signature for polymorphic types, e.g. [('a, 'cmp)
    Ordset)]. *)
module type S_poly2 = sig
  include S_poly2_iter

  val container: ('a, 'cmp) t -> ('a, 'cmp) container
  (** Return container associated with iterator. *)

  val index: ('a, 'cmp) t -> uns
  (** Return iterator index. *)

  val seek: sint -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [(('k,
    'v, 'cmp) Ordmap)]. *)
module type S_poly3_iter = sig
  type ('k, 'v, 'cmp) container
  (** Container type. *)

  type 'k key
  (** Key type. *)

  type 'v value
  (** Value type. *)

  type ('k, 'v, 'cmp) t
  (** Cursor type. *)

  include Cmpable_intf.S_poly3 with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

  val hd: ('k, 'v, 'cmp) container -> ('k, 'v, 'cmp) t
  (** Return head. *)

  val tl: ('k, 'v, 'cmp) container -> ('k, 'v, 'cmp) t
  (** Return tail. *)

  val succ: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** Return successor. *)

  val pred: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** Return predecessor. *)

  val lget: ('k, 'v, 'cmp) t -> ('k key * 'v value)
  (** Return element immediately to left. *)

  val rget: ('k, 'v, 'cmp) t -> ('k key * 'v value)
  (** Return element immediately to right. *)

  val prev: ('k, 'v, 'cmp) t -> ('k key * 'v value) * ('k, 'v, 'cmp) t
  (** [prev t] is equivalent to [lget t, pred t], but potentially more
      efficient. *)

  val next: ('k, 'v, 'cmp) t -> ('k key * 'v value) * ('k, 'v, 'cmp) t
  (** [next t] is equivalent to [rget t, succ t], but potentially more
      efficient. *)
end

(** Cursor functor output signature for polymorphic types, e.g. [('k, 'v, 'cmp)
    Ordmap)]. *)
module type S_poly3 = sig
  include S_poly3_iter

  val container: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) container
  (** Return container associated with iterator. *)

  val index: ('k, 'v, 'cmp) t -> uns
  (** Return iterator index. *)

  val seek: sint -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end
