open Identifiable_intf

module Make (T : I) : S with type t := T.t = struct
  let hash_fold = T.hash_fold

  let pp = T.pp

  let to_string = T.to_string
  let of_string = T.of_string

  include Cmpable.Make(T)
end
