(** Option type.

    The {!type:option} type is useful when a value may or may not be present;
    [Some value] or [None], respectively. *)

(** {1 Type and derivations} *)

(* Our option type must be an alias of OCaml's, lest optional function arguments
 * be incompatible with our option type. *)
type 'a t = 'a option =
| None
| Some of 'a

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

val value: 'a t -> default:'a -> 'a
(** [value t ~default] returns [a] if [t = Some a], [default] otherwise. *)

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

val filter: 'a t -> f:('a -> bool) -> 'a t
(** [filter t ~f] returns [Some a] if [t = Some a] and [f a = true], [None]
    otherwise. *)

val value_map: 'a t -> default:'b -> f:('a -> 'b) -> 'b
(** [value_map t ~default ~f] returns [f a] if [t = Some a], [default]
    otherwise. *)

val merge: 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t
(** [merge t0 t1 ~f] returns [Some (f a0 a1)] if [t0 = Some a0] and [t1 = Some
    a1], otherwise the most preserving of [Some a0], [Some a1], and [None]. *)

val map2: 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
(** [map2 t0 t1 ~f] returns [Some (f a b)] if [t0 = Some a] and [t1 = Some
    b], [None] otherwise. *)
