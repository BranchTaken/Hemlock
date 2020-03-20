(** Either type.

    The {!type:Either.t} type is useful in either-or logic such as set
    partitioning. *)

(** {1 Type} *)

type ('a, 'b) t =
  | First of 'a
  | Second of 'b

(** {1 Comparison} *)

val hash_fold: ('a -> Hash.State.t -> Hash.State.t)
  -> ('b -> Hash.State.t -> Hash.State.t) -> ('a, 'b) t -> Hash.State.t
  -> Hash.State.t
(** [hash_fold hash_fold_a hash_fold_b t state] incorporates the hash of [t]
    into [state] and returns the resulting state.  [First a] is hash-folded into
    the resulting state via [hash_fold_a]; [Second b] is hash-folded into the
    resulting state via [hash_fold_b]. *)

val cmp: ('a -> 'a -> Cmp.t) -> ('b -> 'b -> Cmp.t) -> ('a, 'b) t -> ('a, 'b) t
  -> Cmp.t
(** [cmp cmp_a cmp_b t0 t1] compares [t0] and [t1] using the provided comparison
    functions.  [First a] is less than all [Second] values. *)

include Formattable_intf.S_poly2 with type ('a, 'b) t := ('a, 'b) t

(** {1 Access} *)

val is_first: (_, _) t -> bool
(** [is_first t] returns [true] if [t] is [First _], [false] otherwise. *)

val is_second: (_, _) t -> bool
(** [is_second t] returns [true] if [t] is [Second _], [false] otherwise. *)

(** {1 Mapping and filtering} *)

val swap: ('a, 'b) t -> ('b, 'a) t
(** [swap t] returns [First b] if [t] is [Second b], or [Second a] if [t] is
    [First a]. *)

val value: ('a, 'a) t -> 'a
(** [value t] returns the value associated with [t], where both [First] and
    [Second] have the same associated type. *)

val value_map: first:('a -> 'c) -> second:('b -> 'c) -> ('a, 'b) t -> 'c
(** [value_map t] returns [first a] if [t = First a], or [second b] if [t =
    Second b]. *)

val map: first:('a -> 'c) -> second:('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
(** [map t] returns [First (first a)] if [t = First a], or [Second (second b)]
    if [t = Second b]. *)
