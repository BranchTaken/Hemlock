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
  (** @return Head. *)

  val tl: 'a container -> 'a t
  (** @return Tail. *)

  val succ: 'a t -> 'a t
  (** @return Successor. *)

  val pred: 'a t -> 'a t
  (** @return Predecessor. *)

  val lget: 'a t -> 'a elm
  (** @return Element immediately to left. *)

  val rget: 'a t -> 'a elm
  (** @return Element immediately to right. *)
end

(** Cursor interface for polymorphic types, e.g. [('a array)]. *)
module type S_poly = sig
  include S_poly_iter
  val container: 'a t -> 'a container
  (** @return Container associated with iterator. *)

  val index: 'a t -> uint
  (** @return Iterator index. *)

  val seek: 'a t -> int -> 'a t
  (** @return Iterator at given offset from input iterator. *)
end

(* Cursor iterator interface for monomorphic types, e.g. [string]. *)
module type S_mono_iter = sig
  type container
  type elm
  type t
  include Cmpable_intf.S with type t := t
  val hd: container -> t
  (** @return Head. *)

  val tl: container -> t
  (** @return Tail. *)

  val succ: t -> t
  (** @return Successor. *)

  val pred: t -> t
  (** @return Predecessor. *)

  val lget: t -> elm
  (** @return Element immediately to left. *)

  val rget: t -> elm
  (** @return Element immediately to right. *)
end

(* Cursor iterator interface for monomorphic types, e.g. [string]. *)
module type S_mono = sig
  include S_mono_iter
  val container: t -> container
  (** @return Container associated with iterator. *)

  val index: t -> uint
  (** @return Iterator index. *)

  val seek: t -> int -> t
  (** @return Iterator at given offset from input iterator. *)
end
