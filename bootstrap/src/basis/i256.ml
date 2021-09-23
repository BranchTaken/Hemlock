open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = {w0: u64; w1: u64; w2: u64; w3: u64}
  let word_length = 4L

  let init ~f =
    {w0=f 0L; w1=f 1L; w2=f 2L; w3=f 3L}

  let get i t =
    match i with
    | 0L -> t.w0
    | 1L -> t.w1
    | 2L -> t.w2
    | 3L -> t.w3
    | _ -> not_reached ()
end
include T
include Intw.MakeFI(T)

let to_tup = function
  | {w0; w1; w2; w3} -> (w0, w1, w2, w3)
