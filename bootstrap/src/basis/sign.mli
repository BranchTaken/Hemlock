(** Sign type. *)

type t =
  | Neg  (** Negative. *)
  | Zero (** Zero. *)
  | Pos  (** Positive. *)

include FormattableIntf.SMono with type t := t

val of_sint: int -> t
(** [of_sint x] returns [Neg] if [x < 0], [Zero] if [x = 0], or [Pos] if [x > 0]. *)

val to_sint: t -> int
(** [to_sint t] returns [-1], [0], or [1]. *)

val to_real: t -> float
(** [to_real t] returns [-1.], [0.], or [1.]. *)

val flip: t -> t
(** [flip t] negates [t]. *)

val ( * ): t -> t -> t
(** [t0 * t1] multiplies the signs; e.g. [Neg * Neg] returns [Pos]. *)
