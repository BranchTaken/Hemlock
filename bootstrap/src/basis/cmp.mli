(** Comparison result. *)
type t =
  | Lt (** Less than. *)
  | Eq (** Equal. *)
  | Gt (** Greater than. *)

include FormattableIntf.SMono with type t := t

val is_ge: t -> bool
(** [is_ge t] returns [true] if t is [Gt] or [Eq]; false otherwise. *)

val is_le: t -> bool
(** [is_le t] returns [true] if t is [Lt] or [Eq]; false otherwise. *)

val is_eq: t -> bool
(** [is_eq t] returns [true] if t is [Eq]; false otherwise. *)

val is_gt: t -> bool
(** [is_gt t] returns [true] if t is [Gt]; false otherwise. *)

val is_lt: t -> bool
(** [is_lt t] returns [true] if t is [Lt]; false otherwise. *)

val is_ne: t -> bool
(** [is_ne t] returns [true] if t is [Lt] or [Gt]; false otherwise. *)
