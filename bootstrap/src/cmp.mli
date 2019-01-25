type t =
| Lt
| Eq
| Gt

include Stringable_intf.S with type t := t
