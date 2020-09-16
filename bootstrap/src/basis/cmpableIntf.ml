(** Functor input interface for comparable monomorphic types. *)
module type IMono = sig
  type t

  val cmp: t -> t -> Cmp.t
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
end

(** Functor input interface for comparable types with ranges that contain zero.
*)
module type IMonoZero = sig
  include IMono

  val zero: t
  (** Zero constant. *)
end

(** Functor output signature for infix comparisons on comparable monomorphic
    types. *)
module type SMonoInfix = sig
  type t

  val ( >= ): t -> t -> bool
  (** Returns true if [t0 >= t1]. *)

  val ( <= ): t -> t -> bool
  (** Returns true if [t0 <= t1]. *)

  val ( = ): t -> t -> bool
  (** Returns true if [t0 = t1]. *)

  val ( > ): t -> t -> bool
  (** Returns true if [t0 > t1]. *)

  val ( < ): t -> t -> bool
  (** Returns true if [t0 < t1]. *)

  val ( <> ): t -> t -> bool
  (** Returns true if [t0 <> t1] (i.e. [t0] not equal to [t1]). *)
end

(** Functor output signature for comparisons on comparable monomorphic types. *)
module type SMono = sig
  include IMono
  include SMonoInfix with type t := t

  val ascending: t -> t -> Cmp.t
  (** [ascending t0 t1] compares [t0] and [t1] in ascending order. Equivalent to
      [cmp t0 t1]. *)

  val descending: t -> t -> Cmp.t
  (** [descending t0 t1] compares [t0] and [t1] in descending order. If [cmp]
      provides total ordering, this is equivalent to [cmp t1 t0], but the
      implementation does not assume [cmp] is implemented as such. *)

  val clamp: min:t -> max:t -> t -> t
  (** [clamp ~min ~max t] returns [t] unless it is outside the open-open range
      [~min .. ~max], in which case it returns the nearest end of the range. *)

  val between: low:t -> high:t -> t -> bool
  (** [between ~low ~high t] returns [true] iff [t] is inside the open-open
      range [~low .. ~high]. *)
end

(** Functor output signature for comparable types with ranges that contain zero.
*)
module type SMonoZero = sig
  include IMonoZero
  include SMonoInfix with type t := t

  val is_positive: t -> bool
  (** [is_positive t] returns [true] if [t] is positive, [false] otherwise. NB:
      Positive does {i not} include zero. *)

  val is_non_negative: t -> bool
  (** [is_non_negative t] returns [false] if [t] is negative, [true] otherwise.
  *)

  val is_negative: t -> bool
  (** [is_negative t] returns [true] if [t] is negative, [false] otherwise. *)

  val is_non_positive: t -> bool
  (** [is_non_positive t] returns [false] if [t] is positive, [true] otherwise.
      NB: Positive does {i not} include zero. *)

  val sign: t -> Sign.t
  (** [sign t] returns the sign of [t]. *)
end

(** Key interface required when creating a comparator (see {!module:Cmper}) in
    order to use a type as a key. Only monomorphic keys are supported by the
    keyed container types, so there is no need for a polymorphic version of this
    module. *)
module type Key = sig
  type t
  (** Type being hashed/compared. *)

  val hash_fold: t -> Hash.State.t -> Hash.State.t
  (** [hash_fold a state] incorporates the hash of [a] into [state] and returns
      the resulting state. *)

  include IMono with type t := t
end

(** Functor input interface for comparable polymorphic types, e.g. ['a array].
*)
module type IPoly = sig
  type 'a t

  val cmp: 'a t -> 'a t -> Cmp.t
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
end

(** Functor output signature for infix comparisons on comparable polymorphic
    types, e.g. ['a array]. *)
module type SPoly = sig
  include IPoly

  val ( >= ): 'a t -> 'a t -> bool
  (** Returns true if [t0 >= t1]. *)

  val ( <= ): 'a t -> 'a t -> bool
  (** Returns true if [t0 <= t1]. *)

  val ( = ): 'a t -> 'a t -> bool
  (** Returns true if [t0 = t1]. *)

  val ( > ): 'a t -> 'a t -> bool
  (** Returns true if [t0 > t1]. *)

  val ( < ): 'a t -> 'a t -> bool
  (** Returns true if [t0 < t1]. *)

  val ( <> ): 'a t -> 'a t -> bool
  (** Returns true if [t0 <> t1] (i.e. [t0] not equal to [t1]). *)

  val ascending: 'a t -> 'a t -> Cmp.t
  (** [ascending t0 t1] compares [t0] and [t1] in ascending order. Equivalent to
      [cmp t0 t1]. *)

  val descending: 'a t -> 'a t -> Cmp.t
  (** [descending t0 t1] compares [t0] and [t1] in descending order. If [cmp]
      provides total ordering, this is equivalent to [cmp t1 t0], but the
      implementation does not assume [cmp] is implemented as such. *)

  val clamp: min:'a t -> max:'a t -> 'a t -> 'a t
  (** [clamp ~min ~max t] returns [t] unless it is outside the open-open range
      [~min .. ~max], in which case it returns the nearest end of the range. *)

  val between: low:'a t -> high:'a t -> 'a t -> bool
  (** [between ~low ~high t] returns [true] iff [t] is inside the open-open
      range [~low .. ~high]. *)
end

(** Functor input interface for comparable polymorphic types, e.g. [('a, 'cmp)
    Ordset]. *)
module type IPoly2 = sig
  type ('a, 'cmp) t

  val cmp: ('a, 'cmp) t -> ('a, 'cmp) t -> Cmp.t
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
end

(** Functor output signature for infix comparisons on comparable polymorphic
    types, e.g. [('a, 'cmp) Ordset]. *)
module type SPoly2 = sig
  include IPoly2

  val ( >= ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 >= t1]. *)

  val ( <= ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 <= t1]. *)

  val ( = ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 = t1]. *)

  val ( > ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 > t1]. *)

  val ( < ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 < t1]. *)

  val ( <> ): ('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** Returns true if [t0 <> t1] (i.e. [t0] not equal to [t1]). *)

  val ascending: ('a, 'cmp) t -> ('a, 'cmp) t -> Cmp.t
  (** [ascending t0 t1] compares [t0] and [t1] in ascending order. Equivalent to
      [cmp t0 t1]. *)

  val descending: ('a, 'cmp) t -> ('a, 'cmp) t -> Cmp.t
  (** [descending t0 t1] compares [t0] and [t1] in descending order. If [cmp]
      provides total ordering, this is equivalent to [cmp t1 t0], but the
      implementation does not assume [cmp] is implemented as such. *)

  val clamp: min:('a, 'cmp) t -> max:('a, 'cmp) t -> ('a, 'cmp) t
    -> ('a, 'cmp) t
  (** [clamp ~min ~max t] returns [t] unless it is outside the open-open range
      [~min .. ~max], in which case it returns the nearest end of the range. *)

  val between: low:('a, 'cmp) t -> high:('a, 'cmp) t -> ('a, 'cmp) t -> bool
  (** [between ~low ~high t] returns [true] iff [t] is inside the open-open
      range [~low .. ~high]. *)
end

(** Functor input interface for comparable polymorphic types, e.g. [('k, 'v,
    'cmp) Ordmap]. *)
module type IPoly3 = sig
  type ('k, 'v, 'cmp) t

  val cmp: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> Cmp.t
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
end

(** Functor output signature for infix comparisons on comparable polymorphic
    types, e.g. [('k, 'v, 'cmp) Ordmap]. *)
module type SPoly3 = sig
  include IPoly3

  val ( >= ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 >= t1]. *)

  val ( <= ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 <= t1]. *)

  val ( = ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 = t1]. *)

  val ( > ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 > t1]. *)

  val ( < ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 < t1]. *)

  val ( <> ): ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool
  (** Returns true if [t0 <> t1] (i.e. [t0] not equal to [t1]). *)

  val ascending: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> Cmp.t
  (** [ascending t0 t1] compares [t0] and [t1] in ascending order. Equivalent to
      [cmp t0 t1]. *)

  val descending: ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> Cmp.t
  (** [descending t0 t1] compares [t0] and [t1] in descending order. If [cmp]
      provides total ordering, this is equivalent to [cmp t1 t0], but the
      implementation does not assume [cmp] is implemented as such. *)

  val clamp: min:('k, 'v, 'cmp) t -> max:('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
  (** [clamp ~min ~max t] returns [t] unless it is outside the open-open range
      [~min .. ~max], in which case it returns the nearest end of the range. *)

  val between: low:('k, 'v, 'cmp) t -> high:('k, 'v, 'cmp) t ->
    ('k, 'v, 'cmp) t -> bool
  (** [between ~low ~high t] returns [true] iff [t] is inside the open-open
      range [~low .. ~high]. *)
end
