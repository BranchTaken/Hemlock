(** Ordered set. *)

type ('a, 'cmp) t

include Set_intf.S_ord with type ('a, 'cmp) t := ('a, 'cmp) t
