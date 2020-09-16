(** Unordered set. *)

type ('a, 'cmp) t

include SetIntf.S with type ('a, 'cmp) t := ('a, 'cmp) t
