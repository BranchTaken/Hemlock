(** Unordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.S with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
