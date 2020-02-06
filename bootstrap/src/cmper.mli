(** Comparator type and functors. *)

(** Comparator type, with phantom type that acts as a witness which helps assure
    that multi-container operations can only be performed on containers with
    compatible comparison functions. *)
type ('a, 'witness) t = private {
  cmp: 'a -> 'a -> Cmp.t;
  (** Comparison function. *)

  pp: Format.formatter -> 'a -> unit
  (** Pretty printer function. *)
}

(** Functor input interface for monomorphic comparator types, e.g.
    {!type:string}. *)
module type I_mono = sig
  type t
  include Cmpable_intf.I_mono with type t := t
  include Formattable_intf.S_mono with type t := t
end

(** Functor output signature for monomorphic comparator types, e.g.
    {!type:string}. *)
module type S_mono = sig
  type ('a, 'witness) cmper = ('a, 'witness) t
  (** Comparator type. *)

  type t
  (** Container type. *)

  type cmper_witness
  (** Comparator witness type. *)

  val cmper: (t, cmper_witness) cmper
  (** Comparator. *)
end

(** Functor for monomorphic comparator types, e.g. {!type:string}.  The
    resulting module contains a comparator that can be used in conjunction with
    e.g. {!type:OrdMap}.  The comparator witness assures that multi-container
    operations can only be performed on containers with compatible comparison
    functions. *)
module Make_mono (T : I_mono) : S_mono with type t := T.t

(** Functor input interface for polymorphic comparator types, e.g.
    {!type:'a list}. *)
module type I_poly = sig
  type 'a t
  include Cmpable_intf.I_poly with type 'a t := 'a t
  include Formattable_intf.S_poly with type 'a t := 'a t

  val pp_a: Format.formatter -> 'a -> unit
  (** {!Make_poly} synthesizes a monomorphic [pp] from the composition of [pp]
      and [pp_a].  [pp_a] is the pretty printer for {!type:'a}. *)
end

(** Functor output signature for polymorphic comparator types, e.g.
    {!type:'a list}. *)
module type S_poly = sig
  type ('a, 'witness) cmper = ('a, 'witness) t
  (** Comparator type. *)

  type 'a t
  (** Container type. *)

  type cmper_witness
  (** Comparator witness type. *)

  val cmper: ('a t, cmper_witness) cmper
  (** Comparator. *)
end

(** Functor for polymorphic comparator types, e.g. {!type:'a list}.  The
    resulting module contains a comparator that can be used in conjunction with
    e.g. {!type:OrdMap}.  The comparator witness assures that multi-container
    operations can only be performed on containers with compatible comparison
    functions. *)
module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t
