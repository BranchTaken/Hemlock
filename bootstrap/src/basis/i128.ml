open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = {w0: u64; w1: u64}
  let word_length = 2L

  let init ~f =
    {w0=f 0L; w1=f 1L}

  let get i t =
    match i with
    | 0L -> t.w0
    | 1L -> t.w1
    | _ -> not_reached ()
end
include T
include Intw.MakeFI(T)

let to_tup = function
  | {w0; w1} -> (w0, w1)
