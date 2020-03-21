(** Unordered map. *)

type ('k, 'v, 'cmp) t

include Map_intf.S with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
