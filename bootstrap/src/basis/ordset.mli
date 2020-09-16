(** Ordered set. *)

type ('a, 'cmp) t

include SetIntf.SOrd with type ('a, 'cmp) t := ('a, 'cmp) t
