open Rudiments
open Container_common_intf

(* Polymorphic container, e.g. ('a list). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:'a list}. *)
module type I_poly_array = sig
  include I_poly

  val length: 'a t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
 * {!type:'a list}. *)
module type S_poly_array = sig
  type 'a t
  (** Container type. *)

  val to_array: 'a t -> 'a array
  (** [to_array t] converts [t] to an array. *)
end

(** {!module:S_poly_array_gen} is equivalent to {!module:S_poly_array}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_array_gen = sig
  type 'a t
  type 'a elm
  val to_array: 'a t -> 'a elm array
end

(* Monomorphic, e.g. string. *)

(** Array-related functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type I_mono_array = sig
  include I_mono

  val length: t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for monomorphic containers, e.g.
 * {!type:string}. *)
module type S_mono_array = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val to_array: t -> elm array
  (** [to_array t] converts [t] to an array. *)
end
