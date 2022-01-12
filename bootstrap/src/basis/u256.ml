open RudimentsInt0
open RudimentsFunctions

module T = struct
  module U = struct
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
  include U
  include Intw.MakeFU(U)
end
include T

module UZ = Convert.Make_wU_wZ(T)(Zint)
let trunc_of_zint = UZ.trunc_of_x
let extend_to_zint = UZ.extend_to_x
let narrow_of_zint_opt = UZ.narrow_of_x_opt
let narrow_of_zint_hlt = UZ.narrow_of_x_hlt

module UN = Convert.Make_wU_wN(T)(Nat)
let trunc_of_nat = UN.trunc_of_u
let extend_to_nat = UN.extend_to_u
let narrow_of_nat_opt = UN.narrow_of_u_opt
let narrow_of_nat_hlt = UN.narrow_of_u_hlt

module UX512 = Convert.Make_wU_wX(T)(I512)
let trunc_of_i512 = UX512.trunc_of_x
let extend_to_i512 = UX512.extend_to_x
let narrow_of_i512_opt = UX512.narrow_of_x_opt
let narrow_of_i512_hlt = UX512.narrow_of_x_hlt

module UU512 = Convert.Make_wU_wU(T)(U512)
let trunc_of_u512 = UU512.trunc_of_u
let extend_to_u512 = UU512.extend_to_u
let narrow_of_u512_opt = UU512.narrow_of_u_opt
let narrow_of_u512_hlt = UU512.narrow_of_u_hlt

module UI256 = Convert.Make_wU_wI(T)(I256)
let bits_of_i256 = UI256.bits_of_x
let bits_to_i256 = UI256.bits_to_x
let like_of_i256_opt = UI256.like_of_x_opt
let like_to_i256_opt = UI256.like_to_x_opt
let like_of_i256_hlt = UI256.like_of_x_hlt
let like_to_i256_hlt = UI256.like_to_x_hlt
