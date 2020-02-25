(** Comparison result. *)
type t =
  | Lt (** Less than. *)
  | Eq (** Equal. *)
  | Gt (** Greater than. *)

include Formattable_intf.S_mono with type t := t
