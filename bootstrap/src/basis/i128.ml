open RudimentsInt0
open RudimentsFunctions

module T = struct
  module U = struct
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
  include U
  include Intw.MakeFI(U)
end
include T

module IX512 = Convert.Make_wI_wX(T)(I512)
let trunc_of_i512 = IX512.trunc_of_x
let extend_to_i512 = IX512.extend_to_x
let narrow_of_i512_opt = IX512.narrow_of_x_opt
let narrow_of_i512_hlt = IX512.narrow_of_x_hlt

module IU512 = Convert.Make_wI_wU(T)(U512)
let trunc_of_u512 = IU512.trunc_of_u
let narrow_of_u512_opt = IU512.narrow_of_u_opt
let widen_to_u512_opt = IU512.widen_to_u_opt
let narrow_of_u512_hlt = IU512.narrow_of_u_hlt
let widen_to_u512_hlt = IU512.widen_to_u_hlt

module IX256 = Convert.Make_wI_wX(T)(I256)
let trunc_of_i256 = IX256.trunc_of_x
let extend_to_i256 = IX256.extend_to_x
let narrow_of_i256_opt = IX256.narrow_of_x_opt
let narrow_of_i256_hlt = IX256.narrow_of_x_hlt

module IU256 = Convert.Make_wI_wU(T)(U256)
let trunc_of_u256 = IU256.trunc_of_u
let narrow_of_u256_opt = IU256.narrow_of_u_opt
let widen_to_u256_opt = IU256.widen_to_u_opt
let narrow_of_u256_hlt = IU256.narrow_of_u_hlt
let widen_to_u256_hlt = IU256.widen_to_u_hlt
