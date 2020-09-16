open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = u64 array
  let num_bits = 512
  let of_arr a = a
  let to_arr t = t
end
include T
include Intnw.MakeU(T)

let to_tup = function
  | [|w0; w1; w2; w3; w4; w5; w6; w7|] -> (w0, w1, w2, w3, w4, w5, w6, w7)
  | _ -> not_reached ()
