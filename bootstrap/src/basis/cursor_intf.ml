(** Cursor interfaces. *)

open Rudiments

(** Cursor iterator functor output signature for monomorphic types, e.g.
    [string]. *)
module type S_mono_iter = sig
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

  val pred: t -> t
  (** Return predecessor. *)

  val lget: t -> elm
  (** Return element immediately to left. *)

  val rget: t -> elm
  (** Return element immediately to right. *)
end

(** Cursor functor output signature for monomorphic types, e.g. [string]. *)
module type S_mono = sig
  include S_mono_iter

  val container: t -> container
  (** Return container associated with iterator. *)

  val index: t -> usize
  (** Return iterator index. *)

  val seek: isize -> t -> t
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
end

(** Cursor functor output signature for polymorphic types, e.g. [('a array)]. *)
module type S_poly = sig
  include S_poly_iter

  val container: 'a t -> 'a container
  (** Return container associated with iterator. *)

  val index: 'a t -> usize
  (** Return iterator index. *)

  val seek: isize -> 'a t -> 'a t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end

(** Cursor iterator functor output signature for polymorphic types, e.g. [(('a,
    'cmp) set)]. *)
module type S_poly2_iter = sig
  type ('a, 'b) container
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  type ('a, 'b) t
  (** Cursor type. *)

  include Cmpable_intf.S_poly2 with type ('a, 'b) t := ('a, 'b) t

  val hd: ('a, 'b) container -> ('a, 'b) t
  (** Return head. *)

  val tl: ('a, 'b) container -> ('a, 'b) t
  (** Return tail. *)

  val succ: ('a, 'b) t -> ('a, 'b) t
  (** Return successor. *)

  val pred: ('a, 'b) t -> ('a, 'b) t
  (** Return predecessor. *)

  val lget: ('a, 'b) t -> 'a elm
  (** Return element immediately to left. *)

  val rget: ('a, 'b) t -> 'a elm
  (** Return element immediately to right. *)
end

(** Cursor functor output signature for polymorphic types, e.g. [('a array)]. *)
module type S_poly2 = sig
  include S_poly2_iter

  val container: ('a, 'b) t -> ('a, 'b) container
  (** Return container associated with iterator. *)

  val index: ('a, 'b) t -> usize
  (** Return iterator index. *)

  val seek: isize -> ('a, 'b) t -> ('a, 'b) t
  (** [seek i t] returns an iterator at offset [i] from [t]. *)
end
