open Identifiable_intf

module Make (T : I) : S with type t := T.t
