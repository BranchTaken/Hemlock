(** Option type.

    The {!type:option} type is useful when a value may or may not be present;
    [Some value] or [None], respectively. *)

(** {1 Type} *)

(* Our option type must be an alias of OCaml's, lest optional function arguments
 * be incompatible with our option type. *)
type 'a t = 'a option =
  | None
  | Some of 'a

(** {1 Comparison} *)

val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
  -> Hash.State.t
(** [hash_fold hash_fold_a t state] incorporates the hash of [t] into [state]
    and returns the resulting state.  [Some a] is hash-folded into the resulting
    state via [hash_fold_a]; [None] is hash-folded independently. *)

val cmp: ('a -> 'a -> Cmp.t) -> 'a t -> 'a t -> Cmp.t
(** Compare two options using the value comparison function.  [None] is less
    than all [Some] values. *)

include Formattable_intf.S_poly with type 'a t := 'a t

(** {1 Container} *)

include Container_common_intf.S_poly_length with type 'a t := 'a t
include Container_common_intf.S_poly_fold with type 'a t := 'a t
include Container_array_intf.S_poly_array with type 'a t := 'a t

(** {1 Cursor} *)

(** Cursor that supports {!type:option} access.  [None] is treated as
    zero-length. *)
module Cursor : sig
  type 'a container = 'a t
  type 'a t
  include Cursor_intf.S_poly with type 'a container := 'a container
                              and type 'a elm := 'a
                              and type 'a t := 'a t
end

(** {1 Access} *)

val is_some: 'a t -> bool
(** [is_some t] returns [true] if [t] is [Some _], [false] otherwise. *)

val is_none: 'a t -> bool
(** [is_none t] returns [true] if [t] is [None], [false] otherwise. *)

val value: default:'a -> 'a t -> 'a
(** [value ~default t] returns [a] if [t = Some a], [default] otherwise. *)

val value_hlt: 'a t -> 'a
(** [value t ~default] returns [a] if [t = Some a], halts otherwise. *)

(** {1 Mapping and filtering} *)

val some_if: bool -> 'a -> 'a t
(** [some_if b a] returns [Some a] if [b], [None] otherwise. *)

val both: 'a t -> 'b t -> ('a * 'b) t
(** [both t0 t1] returns [Some (a, b)] if [t0 = Some a] and [t1 = Some b],
    [None] otherwise. *)

val first_some: 'a t -> 'a t -> 'a t
(** [first_some t0 t1] returns [Some a0] if [t0 = Some a0], [Some a1] if [t0 =
    None] and [t1 = Some a1], [None] otherwise. *)

val filter: f:('a -> bool) -> 'a t -> 'a t
(** [filter ~f t] returns [Some a] if [t = Some a] and [f a = true], [None]
    otherwise. *)

val value_map: default:'b -> f:('a -> 'b) -> 'a t -> 'b
(** [value_map ~default ~f t] returns [f a] if [t = Some a], [default]
    otherwise. *)

val merge: f:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** [merge ~f t0 t1] returns [Some (f a0 a1)] if [t0 = Some a0] and [t1 = Some
    a1], otherwise the most preserving of [Some a0], [Some a1], and [None]. *)

val map2: f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 ~f t0 t1] returns [Some (f a b)] if [t0 = Some a] and [t1 = Some b],
    [None] otherwise. *)
