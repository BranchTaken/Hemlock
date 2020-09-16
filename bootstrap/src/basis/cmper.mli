(** Comparator type and functors. *)

type ('a, 'witness) t = private {
  hash_fold: 'a -> Hash.State.t -> Hash.State.t;
  (** Hash-fold function. *)

  cmp: 'a -> 'a -> Cmp.t;
  (** Comparison function. *)

  pp: Format.formatter -> 'a -> unit
  (** Pretty printer function. *)
}
(** Comparator type, with phantom type that acts as a witness which helps assure
    that multi-container operations can only be performed on containers with
    compatible comparison functions. *)

type ('a, 'witness) cmper = ('a, 'witness) t
(** Comparator type. *)

(** Functor input interface for monomorphic comparator types, e.g.
    {!type:string}. *)
module type IMono = sig
  type t
  include CmpableIntf.Key with type t := t
  include FormattableIntf.SMono with type t := t
end

(** Functor output signature for monomorphic comparator types, e.g.
    {!type:string}. *)
module type SMono = sig
  type t
  (** Container type. *)

  type cmper_witness
  (** Comparator witness type. *)

  val cmper: (t, cmper_witness) cmper
  (** Comparator. *)
end

(** Functor for monomorphic comparator types, e.g. {!type:string}. The resulting
    module contains a comparator that can be used in conjunction with e.g.
    {!type:OrdMap}. The comparator witness assures that multi-container
    operations can only be performed on containers with compatible comparison
    functions. *)
module MakeMono (T : IMono) : SMono with type t := T.t

(** Functor input interface for polymorphic comparator types, e.g.
    {!type:'a list}. *)
module type IPoly = sig
  type 'a t
  val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
    -> Hash.State.t
  (** [hash_fold hash_fold_a t state] incorporates the hash of [t] into [state]
      and returns the resulting state. Container elements are sequentially
      hash-folded into the resulting state via [hash_fold_a]. *)

  val hash_fold_a: 'a -> Hash.State.t -> Hash.State.t
  (** [hash_fold_a a state] incorporates the hash of [a] into [state] and
      returns the resulting state. {!MakePoly} synthesizes a monomorphic
      [hash_fold] from the composition of [hash_fold] and [hash_fold_a].
      [hash_fold_a] is the hash-fold function for {!type:'a}. *)

  include CmpableIntf.IPoly with type 'a t := 'a t
  include FormattableIntf.SPoly with type 'a t := 'a t

  val pp_a: Format.formatter -> 'a -> unit
  (** {!MakePoly} synthesizes a monomorphic [pp] from the composition of [pp]
      and [pp_a]. [pp_a] is the pretty printer for {!type:'a}. *)
end

(** Functor output signature for polymorphic comparator types, e.g. {!type:'a
    list}. *)
module type SPoly = sig
  type 'a t
  (** Container type. *)

  type cmper_witness
  (** Comparator witness type. *)

  val cmper: ('a t, cmper_witness) cmper
  (** Comparator. *)
end

(** Functor for polymorphic comparator types, e.g. {!type:'a list}. The
    resulting module contains a comparator that can be used in conjunction with
    e.g. {!type:OrdMap}. The comparator witness assures that multi-container
    operations can only be performed on containers with compatible comparison
    functions. This functor is unusual in that it creates a monomorphic wrapper
    around a specific actualization of a polymorphic type, so that comparators
    are always in effect monomorphic. *)
module MakePoly (T : IPoly) : SPoly with type 'a t := 'a T.t
