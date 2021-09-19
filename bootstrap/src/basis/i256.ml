open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = {w0: u64; w1: u64; w2: u64; w3: u64}
  let word_length = 4

  let init ~f =
    {w0=f 0; w1=f 1; w2=f 2; w3=f 3}

  let get i t =
    match i with
    | 0 -> t.w0
    | 1 -> t.w1
    | 2 -> t.w2
    | 3 -> t.w3
    | _ -> not_reached ()
end
include T
include Intw.MakeFI(T)

let to_tup = function
  | {w0; w1; w2; w3} -> (w0, w1, w2, w3)
