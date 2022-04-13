(** Unordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.S with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

(* Exposed for testing purposes only. *)
val bits_per_level: Uns.t
val bits_per_hash: Uns.t
val hash_fold: ('v -> Hash.State.t -> Hash.State.t) -> ('k, 'v, 'cmp) t -> Hash.State.t
  -> Hash.State.t
val fmt_internals: ?alt:bool -> ?width:int64
  -> ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> ('k, 'v, 'cmp) t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val validate: ('k, 'v, 'cmp) t -> unit
