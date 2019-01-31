type t =
| Lt
| Eq
| Gt

include Sexpable_intf.S with type t := t
