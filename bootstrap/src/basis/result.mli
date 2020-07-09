(** Result type.

    The {!type:result} type is useful for operations which may succeed or fail;
    [Ok ok] or [Error err], respectively. *)

(** {1 Type and derivations} *)

type ('a, 'b) t =
  | Ok of 'a    (** Success, with result. *)
  | Error of 'b (** Failure, with error. *)

include Formattable_intf.S_poly2 with type ('a, 'b) t := ('a, 'b) t

(** {1 Creation} *)

val ok_if: bool -> error:'b -> (unit, 'b) t
(** [ok_if b ~error] returns [Ok ()] if [b = true], [Error error] otherwise. *)

val error_if: bool -> ok:'a -> ('a, unit) t
(** [error_if a ~ok] returns [Error ()] if [b = true], [Ok ok] otherwise. *)

(** {1 Access} *)

val is_ok: ('a, 'b) t -> bool
(** [is_ok t] returns [true] if [t = Ok _], [false] otherwise. *)

val is_error: ('a, 'b) t -> bool
(** [is_error t] returns [true] if [t = Error _], [false] otherwise. *)

val ok_opt: ('a, _) t -> 'a option
(** [ok_opt a] returns [Some a] if [t = Ok a], [None] otherwise. *)

val ok_hlt: ('a, 'b) t -> 'a
(** [ok_opt a] returns [Some a] if [t = Ok a], halts otherwise. *)

val error_opt: (_, 'b) t -> 'b option
(** [error_opt b] returns [Some b] if [t = Error b], [None] otherwise. *)

val error_hlt: (_, 'b) t -> 'b
(** [error_opt b] returns [Some b] if [t = Error b], halts otherwise. *)

val all: ('a, 'b) t list -> ('a list, 'b list) t
(** Convert a list of successes to a success list comprising all successes, or
    return a failure list comprising all failures.  Not tail-recursive. *)

val all_hlt: ('a, 'b) t list -> 'a list
(** Convert a list of successes to a success list comprising all successes, or
    halt.  Not tail-recursive. *)

(** Mapping and filtering. *)

val ok_ignore: (_, 'b) t list -> (unit, 'b list) t
(** Convert a list of successes to [unit], or return a failure list comprising
    all failures.  Not tail-recursive. *)

val ok_ignore_hlt: (_, 'b) t list -> unit
(** Convert a list of successes to [unit], or halt.  Not tail-recursive. *)

val error_ignore: ('a, _) t list -> ('a list, unit) t
(** Convert a list of failures to [unit], or return a success list comprising
    all successes.  Not tail-recursive. *)

val error_ignore_hlt: ('a, _) t list -> unit
(** Convert a list of failures to [unit], or halt.  Not tail-recursive. *)

val map_ok: f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
(** [map_ok ~f t] returns [Ok (f a)] if [t = Ok a], or [Error b] if [t = Error
    b]. *)

val map_error: f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** [map_error ~f t] returns [Error (f b)] if [t = Error b], or [Ok a] if [t =
    Ok a]. *)

val merge: ok:('a -> 'a -> 'a) -> error:('b -> 'b -> 'b) -> ('a, 'b) t
  -> ('a, 'b) t -> ('a, 'b) t
(** [merge ~ok ~error t0 t1] returns [Ok (ok a0 a1)] if [t0 = Ok a0] and [t1 =
    Ok a1], [Error (error b0 b1)] if [t0 = Error b0] and [t1 = Error b1], or
    [Error b0] or [Error b1] if [t0 = Error b0] or [t1 = Error b1],
    respectively. *)
