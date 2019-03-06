(* Partial Rudiments. *)
type 'a array = 'a Array.t

open Container_common_intf

(* Polymorphic container, e.g. ('a array). *)

module type I_poly_array = sig
  include I_poly
  val length: 'a t -> int
end

module type S_poly_array_gen = sig
  type 'a t
  type 'a elm
  val to_array: 'a t -> 'a elm array
end

module type S_poly_array = sig
  type 'a t
  val to_array: 'a t -> 'a array
end

(* Monomorphic, e.g. string. *)

module type I_mono_array = sig
  include I_mono
  val length: t -> int
end

module type S_mono_array = sig
  type t
  type elm
  val to_array: t -> elm array
end
