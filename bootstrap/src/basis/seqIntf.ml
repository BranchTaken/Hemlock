(** Sequence functor interfaces and signatures. *)

open RudimentsInt

(** Definite sequence functor input interface for monomorphic containers, e.g.
    {!type:string}. *)
module type IMonoDef = sig
  type t
  (** Sequence type. *)

  type elm
  (** Element type. *)

  val length: t -> uns
  (** Remaining sequence length. *)

  val next: t -> elm * t
  (** Return next element and sequence absent the element. *)
end

(** Indefinite sequence functor input interface for monomorphic containers, e.g.
    {!type:byte list}. *)
module type IMonoIndef = sig
  type t
  (** Sequence type. *)

  type elm
  (** Element type. *)

  val next: t -> (elm * t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Definite sequence functor output signature for monomorphic containers, e.g.
    {!type:string}. *)
module type SMonoDef = sig
  include IMonoDef

  val next_opt: t -> (elm * t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Indefinite sequence functor output signature for monomorphic containers,
    e.g. {!type:byte list}. *)
module type SMonoIndef = sig
  include IMonoIndef
end

(** Definite sequence functor input interface for polymorphic containers, e.g.
    {!type:'a array}. *)
module type IPolyDef = sig
  type 'a t
  (** Sequence type. *)

  type 'a elm
  (** Element type. *)

  val length: 'a t -> uns
  (** Remaining sequence length. *)

  val next: 'a t -> 'a elm * 'a t
  (** Return next element and sequence absent the element. *)
end

(** Indefinite sequence functor input interface for polymorphic containers, e.g.
    {!type:'a list}. *)
module type IPolyIndef = sig
  type 'a t
  (** Sequence type. *)

  type 'a elm
  (** Element type. *)

  val next: 'a t -> ('a elm * 'a t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Definite sequence functor output signature for polymorphic containers, e.g.
    {!type:'a array}. *)
module type SPolyDef = sig
  include IPolyDef

  val next_opt: 'a t -> ('a elm * 'a t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

(** Indefinite sequence functor output signature for polymorphic containers,
    e.g. {!type:'a list}. *)
module type SPolyIndef = sig
  include IPolyIndef
end

(** Definite sequence functor input interface for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type IPoly2Def = sig
  type ('a, 'cmp) t
  (** Sequence type. *)

  type 'a elm
  (** Element type. *)

  val length: ('a, 'cmp) t -> uns
  (** Remaining sequence length. *)

  val next: ('a, 'cmp) t -> 'a elm * ('a, 'cmp) t
  (** Return next element and sequence absent the element. *)
end

(** Definite sequence functor output signature for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type SPoly2Def = sig
  include IPoly2Def

  val next_opt: ('a, 'cmp) t -> ('a elm * ('a, 'cmp) t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

module type IPoly2Fold2 = sig
  type ('a, 'cmp) container
  (** Container type. *)

  include SPoly2Def

  val cmper: ('a, 'cmp) container -> ('a elm, 'cmp) Cmper.t

  val cmp: ('a, 'cmp) Cmper.t -> 'a -> 'a -> Cmp.t

  val init: ('a, 'cmp) container -> ('a, 'cmp) t
  (** [init container] returns an initialized sequence. *)
end

module type SPoly2Fold2 = sig
  type ('a, 'cmp) t
  (** Container type. *)

  type 'a elm
  (** Element type. *)

  val fold2_until: init:'accum
    -> f:('accum -> 'a elm option -> 'a elm option -> 'accum * bool)
    -> ('a, 'cmp) t -> ('a, 'cmp) t -> 'accum
  (** [fold2_until ~init ~f t0 t1] folds over the union of [t0] and [t1] from
      left to right if ordered, or arbitrarily if unordered, and calls [~f accum
      elm0_opt elm1_opt] once for each element in the union such that if the
      element is absent from one of the input sets, the corresponding parameter
      is [None]. Folding terminates early if [~f] returns [(_, true)]. O(m+n)
      time complexity, where m and n are the input set lengths. *)

  val fold2: init:'accum
    -> f:('accum -> 'a elm option -> 'a elm option -> 'accum) -> ('a, 'cmp) t
    -> ('a, 'cmp) t -> 'accum
  (** [fold2 ~init ~f t0 t1] folds over the union of [t0] and [t1] from left to
      right if ordered, or arbitrarily if unordered, and calls [~f accum
      elm0_opt elm1_opt] once for each element in the union such that if the
      element is absent from one of the input sets, the corresponding parameter
      is [None]. Θ(m+n) time complexity, where m and n are the input set
      lengths. *)

  val iter2: f:('a elm option -> 'a elm option -> unit) -> ('a, 'cmp) t ->
    ('a, 'cmp) t -> unit
  (** [iter2 ~f t0 t1] iterates over the union of [t0] and [t1] from left to
      right if ordered, or arbitrarily if unordered, and calls [~f elm0_opt
      elm1_opt] once for each element in the union, such that if the element is
      absent from one of the input sets, the corresponding parameter is [None].
      Θ(m+n) time complexity, where m and n are the input set lengths. *)
end

(** Definite sequence functor input interface for polymorphic containers, e.g.
    {!type:('k, 'v, 'cmp) Ordmap}. *)
module type IPoly3Def = sig
  type ('k, 'v, 'cmp) t
  (** Sequence type. *)

  type 'k key
  (** Key type. *)

  type 'v value
  (** Value type. *)

  val length: ('k, 'v, 'cmp) t -> uns
  (** Remaining sequence length. *)

  val next: ('k, 'v, 'cmp) t -> ('k key * 'v value) * ('k, 'v, 'cmp) t
  (** Return next element and sequence absent the element. *)
end

(** Definite sequence functor output signature for polymorphic containers, e.g.
    {!type:('a, 'cmp) Ordset}. *)
module type SPoly3Def = sig
  include IPoly3Def

  val next_opt: ('k, 'v, 'cmp) t -> (('k key * 'v value) *
      ('k, 'v, 'cmp) t) option
  (** Return next element and sequence absent the element, or [None] if sequence
      is empty. *)
end

module type IPoly3Fold2 = sig
  type ('k, 'v, 'cmp) container
  (** Container type. *)

  include SPoly3Def

  val cmper: ('k, 'v, 'cmp) container -> ('k key, 'cmp) Cmper.t

  val cmp: ('k, 'cmp) Cmper.t -> 'k -> 'k -> Cmp.t

  val init: ('k, 'v, 'cmp) container -> ('k, 'v, 'cmp) t
  (** [init container] returns an initialized sequence. *)
end

module type SPoly3Fold2 = sig
  type ('k, 'v, 'cmp) t
  (** Container type. *)

  type 'k key
  (** Key type. *)

  type 'v value
  (** Value type. *)

  val fold2_until: init:'accum -> f:('accum -> ('k key * 'v value) option ->
    ('k key * 'v value) option -> 'accum * bool) -> ('k, 'v, 'cmp) t ->
    ('k, 'v, 'cmp) t -> 'accum
  (** [fold2_until ~init ~f t0 t1] folds over the union of [t0] and [t1] from
      left to right if ordered, or arbitrarily if unordered, and calls [~f accum
      elm0_opt elm1_opt] once for each element in the union such that if the
      element is absent from one of the input sets, the corresponding parameter
      is [None]. Folding terminates early if [~f] returns [(_, true)]. O(m+n)
      time complexity, where m and n are the input set lengths. *)

  val fold2: init:'accum -> f:('accum -> ('k key * 'v value) option ->
    ('k key * 'v value) option -> 'accum) -> ('k, 'v, 'cmp) t ->
    ('k, 'v, 'cmp) t -> 'accum
  (** [fold2 ~init ~f t0 t1] folds over the union of [t0] and [t1] from left to
      right if ordered, or arbitrarily if unordered, and calls [~f accum
      elm0_opt elm1_opt] once for each element in the union such that if the
      element is absent from one of the input sets, the corresponding parameter
      is [None]. Θ(m+n) time complexity, where m and n are the input set
      lengths. *)

  val iter2: f:(('k key * 'v value) option -> ('k key * 'v value) option ->
    unit) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> unit
  (** [iter2 ~f t0 t1] iterates over the union of [t0] and [t1] from left to
      right if ordered, or arbitrarily if unordered, and calls [~f elm0_opt
      elm1_opt] once for each element in the union, such that if the element is
      absent from one of the input sets, the corresponding parameter is [None].
      Θ(m+n) time complexity, where m and n are the input set lengths. *)
end
