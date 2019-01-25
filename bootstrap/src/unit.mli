type t = unit [@@deriving compare]

include Cmpable_intf.I with type t := t
include Sexpable_intf.S with type t := t
include Stringable_intf.S with type t := t
