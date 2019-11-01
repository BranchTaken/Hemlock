(* Partial Rudiments. *)
module Int = I63
module Uint = U63
type uint = Uint.t

(** Cursor iterator interface for polymorphic types, e.g. [('a array)]. *)
module type S_poly_iter = sig
  type 'a container
  type 'a t
  type 'a elm
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

(** Cursor interface for polymorphic types, e.g. [('a array)]. *)
module type S_poly = sig
  include S_poly_iter

  val container: 'a t -> 'a container
  (** Return container associated with iterator. *)

  val index: 'a t -> uint
  (** Return iterator index. *)

  val seek: 'a t -> int -> 'a t
  (** Return iterator at given offset from input iterator. *)
end

(* Cursor iterator interface for monomorphic types, e.g. [string]. *)
module type S_mono_iter = sig
  type container
  type elm
  type t
  include Cmpable_intf.S with type t := t
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

(* Cursor iterator interface for monomorphic types, e.g. [string]. *)
module type S_mono = sig
  include S_mono_iter
  val container: t -> container
  (** Return container associated with iterator. *)

  val index: t -> uint
  (** Return iterator index. *)

  val seek: t -> int -> t
  (** Return iterator at given offset from input iterator. *)
end
