(* Partial Rudiments. *)
module Int = I63
module Uint = U63
type int = Int.t
type uint = Uint.t

(* Polymorphic, e.g. ('a array). *)
module type S_poly_iter = sig
  type 'a container
  type 'a t
  type 'a elm
  include Cmpable_intf.S_poly with type 'a t := 'a t
  val hd: 'a container -> 'a t
  val tl: 'a container -> 'a t
  val succ: 'a t -> 'a t
  val pred: 'a t -> 'a t
  val lget: 'a t -> 'a elm
  val rget: 'a t -> 'a elm
end

module type S_poly = sig
  include S_poly_iter
  val container: 'a t -> 'a container
  val index: 'a t -> uint
  val seek: 'a t -> int -> 'a t
end

(* Monomorphic, e.g. string. *)

module type S_mono_iter = sig
  type container
  type elm
  type t
  include Cmpable_intf.S with type t := t
  val hd: container -> t
  val tl: container -> t
  val succ: t -> t
  val pred: t -> t
  val lget: t -> elm
  val rget: t -> elm
end

module type S_mono = sig
  include S_mono_iter
  val container: t -> container
  val index: t -> uint
  val seek: t -> int -> t
end
