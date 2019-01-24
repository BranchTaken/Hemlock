type t = bool [@@deriving compare]

include Cmpable_intf.I with type t := t
include Cmpable_intf.S_eq with type t := t
include Sexpable_intf.S with type t := t
include Intable_intf.S with type t := t
include Stringable_intf.S with type t := t
