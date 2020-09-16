open Rudiments0
open ContainerCommonIntf

(* Monomorphic, e.g. string. *)

(** Array-related functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type IMonoArray = sig
  include IMono

  val length: t -> uns
  (** Container length. *)
end

(** Array-related functor output signature for monomorphic containers, e.g.
    {!type:string}. *)
module type SMonoArray = sig
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
module type IPolyArray = sig
  include IPoly

  val length: 'a t -> uns
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:'a list}. *)
module type SPolyArray = sig
  type 'a t
  (** Container type. *)

  val to_array: 'a t -> 'a array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:SPolyArrayGen} is equivalent to {!module:SPolyArray}, except that
    {!type:'a elm} is explicit. This near-identical signature exists exclusively
    to enable functor implementation. *)
module type SPolyArrayGen = sig
  type 'a t
  type 'a elm
  val to_array: 'a t -> 'a elm array
end

(* Polymorphic container, e.g. (('a, 'cmp) Ordset). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type IPoly2Array = sig
  include IPoly2

  val length: ('a, 'cmp) t -> uns
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type SPoly2Array = sig
  type ('a, 'cmp) t
  (** Container type. *)

  val to_array: ('a, 'cmp) t -> 'a array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:SPoly2ArrayGen} is equivalent to {!module:SPoly2Array}, except that
    {!type:'a elm} is explicit. This near-identical signature exists exclusively
    to enable functor implementation. *)
module type SPoly2ArrayGen = sig
  type ('a, 'cmp) t
  type 'a elm
  val to_array: ('a, 'cmp) t -> 'a elm array
end

(* Polymorphic container, e.g. (('k, 'v, 'cmp) Ordmap). *)

(** Array-related functor input interface for polymorphic containers, e.g.
    {!type:('k, 'v, 'cmp) Ordmap}. *)
module type IPoly3Array = sig
  include IPoly3

  val length: ('k, 'v, 'cmp) t -> uns
  (** Container length. *)
end

(** Array-related functor output signature for polymorphic containers, e.g.
    {!type:('k, 'v, 'cmp) Ordmap}. *)
module type SPoly3Array = sig
  type ('k, 'v, 'cmp) t
  (** Container type. *)

  val to_array: ('k, 'v, 'cmp) t -> ('k * 'v) array
  (** [to_array t] converts the elements of [t] from left to right, to an array.
  *)
end

(** {!module:SPoly3ArrayGen} is equivalent to {!module:SPoly3Array}, except that
    {!type:'k key} and {!type:'v value} are explicit. This near-identical
    signature exists exclusively to enable functor implementation. *)
module type SPoly3ArrayGen = sig
  type ('k, 'v, 'cmp) t
  type 'k key
  type 'v value
  val to_array: ('k, 'v, 'cmp) t -> ('k key * 'v value) array
end
