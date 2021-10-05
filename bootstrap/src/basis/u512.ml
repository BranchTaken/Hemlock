open RudimentsInt0
open RudimentsFunctions

module T = struct
  module U = struct
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
  include U
  include Intw.MakeFU(U)
end
include T

module UI512 = Convert.Make_wU_wI(T)(I512)
let bits_of_i512 = UI512.bits_of_x
let bits_to_i512 = UI512.bits_to_x
let like_of_i512_opt = UI512.like_of_x_opt
let like_to_i512_opt = UI512.like_to_x_opt
let like_of_i512_hlt = UI512.like_of_x_hlt
let like_to_i512_hlt = UI512.like_to_x_hlt
