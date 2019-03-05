(* Monomorphic, e.g. string. *)

module type I_mono_def = sig
  type t
  type elm
  val length: t -> int
  val next: t -> elm * t
end

module type I_mono_indef = sig
  type t
  type elm
  val next: t -> (elm * t) option
end

module type S_mono_def = sig
  include I_mono_def
  val next_opt: t -> (elm * t) option
end

module type S_mono_indef = sig
  include I_mono_indef
end

(* Polymorphic, e.g. ('a array). *)

module type I_poly_def = sig
  type 'a t
  val length: 'a t -> int
  val next: 'a t -> 'a * 'a t
end

module type I_poly_indef = sig
  type 'a t
  val next: 'a t -> ('a * 'a t) option
end

module type S_poly_def = sig
  include I_poly_def
  val next_opt: 'a t -> ('a * 'a t) option
end

module type S_poly_indef = sig
  include I_poly_indef
end
