(** Comparison result. *)
type t =
| Lt (** Less than. *)
| Eq (** Equal. *)
| Gt (** Greater than. *)

val pp: Format.formatter -> t -> unit
(** [pp ppf t] prints a representation of [t] to the pretty printing formatter,
    [ppf].  This function is intended for use with the [%a] format specifier to
    {!Format.printf}. *)
