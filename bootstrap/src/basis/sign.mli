(** Sign type. *)

type t =
  | Neg  (** Negative. *)
  | Zero (** Zero. *)
  | Pos  (** Positive. *)

include Formattable_intf.S_mono with type t := t

val of_isize: int -> t
(** [of_isize x] returns [Neg] if [x < 0], [Zero] if [x = 0], or [Pos] if [x >
    0]. *)

val to_isize: t -> int
(** [to_isize t] returns [-1], [0], or [1]. *)

val to_float: t -> float
(** [to_float t] returns [-1.], [0.], or [1.]. *)

val flip: t -> t
(** [flip t] negates [t]. *)

val ( * ): t -> t -> t
(** [t0 * t1] multiplies the signs; e.g. [Neg * Neg] returns [Pos]. *)
