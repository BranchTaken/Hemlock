open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = {w0: u64; w1: u64; w2: u64; w3: u64;
    w4: u64; w5: u64; w6: u64; w7: u64}
  let word_length = 8

  let init ~f =
    {w0=f 0; w1=f 1; w2=f 2; w3=f 3; w4=f 4; w5=f 5; w6=f 6; w7=f 7}

  let get i t =
    match i with
    | 0 -> t.w0
    | 1 -> t.w1
    | 2 -> t.w2
    | 3 -> t.w3
    | 4 -> t.w4
    | 5 -> t.w5
    | 6 -> t.w6
    | 7 -> t.w7
    | _ -> not_reached ()
end
include T
include Intw.MakeFU(T)

let to_tup = function
  | {w0; w1; w2; w3; w4; w5; w6; w7} -> (w0, w1, w2, w3, w4, w5, w6, w7)
