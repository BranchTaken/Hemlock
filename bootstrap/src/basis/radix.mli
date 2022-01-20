(** Radix (base) type. *)

open RudimentsInt0

include (module type of Radix0)

include FormattableIntf.SMono with type t := t

val to_uns: t -> uns
(** [to_uns t] returns [2], [8], [10], or [16]. *)
