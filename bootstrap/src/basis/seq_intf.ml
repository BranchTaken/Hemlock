(** Sequence functor interfaces and signatures. *)

open Rudiments

(** Definite sequence functor input interface for polymorphic containers, e.g.
    {!type:'a array}. *)
module type I_poly_def = sig
  type 'a t
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  val length: 'a t -> usize
  (** Remaining sequence length. *)

  val next: 'a t -> 'a elm * 'a t
  (** Return next element and sequence absent the element. *)
end

(** Indefinite sequence functor input interface for polymorphic containers, e.g.
    {!type:'a list}. *)
module type I_poly_indef = sig
  type 'a t
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  val next: 'a t -> ('a elm * 'a t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Definite sequence functor output signature for polymorphic containers, e.g.
    {!type:'a array}. *)
module type S_poly_def = sig
  include I_poly_def

  val next_opt: 'a t -> ('a elm * 'a t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Indefinite sequence functor output signature for polymorphic containers,
    e.g.  {!type:'a list}. *)
module type S_poly_indef = sig
  include I_poly_indef
end

(** Definite sequence functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type I_mono_def = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val length: t -> usize
  (** Remaining sequence length. *)

  val next: t -> elm * t
  (** Return next element and sequence absent the element. *)
end

(** Indefinite sequence functor input interface for monomorphic containers, e.g.
    {!type:byte list}. *)
module type I_mono_indef = sig
  type t
  (** Container type. *)

  type elm
  (** Element type. *)

  val next: t -> (elm * t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Definite sequence functor output signature for monomorphic containers, e.g.
    {!type:string}. *)
module type S_mono_def = sig
  include I_mono_def

  val next_opt: t -> (elm * t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Indefinite sequence functor output signature for monomorphic containers,
    e.g.  {!type:byte list}. *)
module type S_mono_indef = sig
  include I_mono_indef
end
