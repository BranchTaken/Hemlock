open Rudiments
open Container_common_intf

(* Monomorphic, e.g. string. *)

(** Array-related functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type I_mono_array = sig
  include I_mono

  val length: t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for monomorphic containers, e.g.
    {!type:string}. *)
module type S_mono_array = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val to_array: t -> elm array
  (** [to_array t] converts [t] to an array. *)
end

(* Polymorphic container, e.g. ('a list). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:'a list}. *)
module type I_poly_array = sig
  include I_poly

  val length: 'a t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:'a list}. *)
module type S_poly_array = sig
  type 'a t
  (** Container type. *)

  val to_array: 'a t -> 'a array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:S_poly_array_gen} is equivalent to {!module:S_poly_array}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly_array_gen = sig
  type 'a t
  type 'a elm
  val to_array: 'a t -> 'a elm array
end

(* Polymorphic container, e.g. (('a, 'cmp) Ordset). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type I_poly2_array = sig
  include I_poly2

  val length: ('a, 'cmp) t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type S_poly2_array = sig
  type ('a, 'cmp) t
  (** Container type. *)

  val to_array: ('a, 'cmp) t -> 'a array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:S_poly2_array_gen} is equivalent to {!module:S_poly2_array}, except
    that {!type:'a elm} is explicit.  This near-identical signature exists
    exclusively to enable functor implementation. *)
module type S_poly2_array_gen = sig
  type ('a, 'cmp) t
  type 'a elm
  val to_array: ('a, 'cmp) t -> 'a elm array
end

(* Polymorphic container, e.g. (('k, 'v, 'cmp) Ordmap). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:('k, 'v, 'cmp) Ordmap}. *)
module type I_poly3_array = sig
  include I_poly3

  val length: ('k, 'v, 'cmp) t -> usize
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:('k, 'v, 'cmp) Ordmap}. *)
module type S_poly3_array = sig
  type ('k, 'v, 'cmp) t
  (** Container type. *)

  val to_array: ('k, 'v, 'cmp) t -> ('k * 'v) array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:S_poly3_array_gen} is equivalent to {!module:S_poly3_array}, except
    that {!type:'k key} and {!type:'v value} are explicit.  This near-identical
    signature exists exclusively to enable functor implementation. *)
module type S_poly3_array_gen = sig
  type ('k, 'v, 'cmp) t
  type 'k key
  type 'v value
  val to_array: ('k, 'v, 'cmp) t -> ('k key * 'v value) array
end
