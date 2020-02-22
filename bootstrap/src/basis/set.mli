(** Unordered set. *)

type ('a, 'cmp) t

include Set_intf.S with type ('a, 'cmp) t := ('a, 'cmp) t
