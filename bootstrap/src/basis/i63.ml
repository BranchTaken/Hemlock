(* Partial Rudiments. *)
include RudimentsInt0

let of_int t =
  sint_of_int t

let kv t =
  sint_of_int t

module T = struct
  type t = sint
  let bit_length = Sys.int_size
end
include T
include Intnb.MakeI(T)
