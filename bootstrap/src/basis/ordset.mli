(** Ordered set. *)

type ('a, 'cmp) t

include SetIntf.SOrdPoly with type ('a, 'cmp) t := ('a, 'cmp) t
