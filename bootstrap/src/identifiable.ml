open Identifiable_intf

module Make (T : I) : S with type t := T.t = struct
  let hash = T.hash

  let t_of_sexp = T.t_of_sexp
  let sexp_of_t = T.sexp_of_t

  let to_string = T.to_string
  let of_string = T.of_string

  include Cmpable.Make(T)
end
