(** Comparison result. *)
type t =
| Lt (** Less than. *)
| Eq (** Equal. *)
| Gt (** Greater than. *)

include Sexpable_intf.S with type t := t
