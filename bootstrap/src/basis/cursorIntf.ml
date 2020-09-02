(** Cursor interfaces. *)

open Rudiments0

(** Cursor iterator functor output signature for monomorphic types, e.g.
    [string]. *)
module type SMonoIter = sig
  type container
  (** Container type. *)

  type elm
  (** Element type. *)

  type t
  (** Cursor type. *)

  include CmpableIntf.SMono with type t := t

  val hd: container -> t
  (** Return head. *)

  val tl: container -> t
  (** Return tail. *)

  val succ: t -> t
  (** Return successor. *)

  val pred: t -> t
  (** Return predecessor. *)

  val lget: t -> elm
  (** Return element immediately to left. *)

  val rget: t -> elm
  (** Return element immediately to right. *)

  val prev: t -> elm * t
  (** [prev t] is equivalent to [lget t, pred t], but potentially more
      efficient. *)

  val next: t -> elm * t
  (** [next t] is equivalent to [rget t, succ t], but potentially more
      efficient. *)
end

(** Cursor functor output signature for monomorphic types, e.g. [string]. *)
module type SMono = sig
  include SMonoIter

  val container: t -> container
  (** Return container associated with iterator. *)

  val index: t -> uns
  (** Return iterator index. *)

  val seek: sint -> t -> t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [('a
    array)]. *)
module type SPolyIter = sig
  type 'a container
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  type 'a t
  (** Cursor type. *)

  include CmpableIntf.SPoly with type 'a t := 'a t

  val hd: 'a container -> 'a t
  (** Return head. *)

  val tl: 'a container -> 'a t
  (** Return tail. *)

  val pred: 'a t -> 'a t
  (** Return predecessor. *)

  val succ: 'a t -> 'a t
  (** Return successor. *)

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
module type SPoly = sig
  include SPolyIter

  val container: 'a t -> 'a container
  (** Return container associated with iterator. *)

  val index: 'a t -> uns
  (** Return iterator index. *)

  val seek: sint -> 'a t -> 'a t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [(('a,
    'cmp) Ordset)]. *)
module type SPoly2Iter = sig
  type ('a, 'cmp) container
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  type ('a, 'cmp) t
  (** Cursor type. *)

  include CmpableIntf.SPoly2 with type ('a, 'cmp) t := ('a, 'cmp) t

  val hd: ('a, 'cmp) container -> ('a, 'cmp) t
  (** Return head. *)

  val tl: ('a, 'cmp) container -> ('a, 'cmp) t
  (** Return tail. *)

  val pred: ('a, 'cmp) t -> ('a, 'cmp) t
  (** Return predecessor. *)

  val succ: ('a, 'cmp) t -> ('a, 'cmp) t
  (** Return successor. *)

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
module type SPoly2 = sig
  include SPoly2Iter

  val container: ('a, 'cmp) t -> ('a, 'cmp) container
  (** Return container associated with iterator. *)

  val index: ('a, 'cmp) t -> uns
  (** Return iterator index. *)

  val seek: sint -> ('a, 'cmp) t -> ('a, 'cmp) t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [(('k,
    'v, 'cmp) Ordmap)]. *)
module type SPoly3Iter = sig
  type ('k, 'v, 'cmp) container
  (** Container type. *)

  type 'k key
  (** Key type. *)

  type 'v value
  (** Value type. *)

  type ('k, 'v, 'cmp) t
  (** Cursor type. *)

  include CmpableIntf.SPoly3 with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

  val hd: ('k, 'v, 'cmp) container -> ('k, 'v, 'cmp) t
  (** Return head. *)

  val tl: ('k, 'v, 'cmp) container -> ('k, 'v, 'cmp) t
  (** Return tail. *)

  val pred: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** Return predecessor. *)

  val succ: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** Return successor. *)

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
module type SPoly3 = sig
  include SPoly3Iter

  val container: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) container
  (** Return container associated with iterator. *)

  val index: ('k, 'v, 'cmp) t -> uns
  (** Return iterator index. *)

  val seek: sint -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end
