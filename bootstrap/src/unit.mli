type t = unit [@@deriving compare]

include Identifiable_intf.S with type t := t
