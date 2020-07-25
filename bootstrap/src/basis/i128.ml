open Rudiments_int
open Rudiments_functions

module T = struct
  type t = u64 array
  let num_bits = 128
  let of_arr a = a
  let to_arr t = t
end
include T
include Intnw.Make_i(T)

let to_tup = function
  | [|w0; w1|] -> (w0, w1)
  | _ -> not_reached ()
