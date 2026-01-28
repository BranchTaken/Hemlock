(** Unordered set. *)

type ('a, 'cmp) t

include SetIntf.SPoly with type ('a, 'cmp) t := ('a, 'cmp) t
