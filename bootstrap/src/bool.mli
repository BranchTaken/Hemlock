type t = bool [@@deriving sexp, compare]

include Intable_intf.S with type t := t
include Stringable_intf.S with type t := t
include Cmpable_intf.I with type t := t
include Cmpable_intf.S_eq with type t := t
