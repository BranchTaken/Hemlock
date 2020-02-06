(** Unit type. *)

type t = unit

include Identifiable_intf.S with type t := t
include Stringable_intf.S with type t := t
