(** Unit type. *)

type t = unit

include IdentifiableIntf.S with type t := t
include StringableIntf.S with type t := t
