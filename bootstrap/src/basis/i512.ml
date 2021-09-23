open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = {w0: u64; w1: u64; w2: u64; w3: u64; w4: u64; w5: u64; w6: u64; w7: u64}
  let word_length = 8L

  let init ~f =
    {w0=f 0L; w1=f 1L; w2=f 2L; w3=f 3L; w4=f 4L; w5=f 5L; w6=f 6L; w7=f 7L}

  let get i t =
    match i with
    | 0L -> t.w0
    | 1L -> t.w1
    | 2L -> t.w2
    | 3L -> t.w3
    | 4L -> t.w4
    | 5L -> t.w5
    | 6L -> t.w6
    | 7L -> t.w7
    | _ -> not_reached ()
end
include T
include Intw.MakeFI(T)

let to_tup = function
  | {w0; w1; w2; w3; w4; w5; w6; w7} -> (w0, w1, w2, w3, w4, w5, w6, w7)
