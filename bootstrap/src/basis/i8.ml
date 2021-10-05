(* Partial Rudiments. *)
open RudimentsInt0

module T = struct
  module U = struct
    type t = sint
    let bit_length = 8L
  end
  include U
  include Intnb.MakeI(U)
end
include T

module IX512 = Convert.Make_nbI_wX(T)(I512)
let trunc_of_i512 = IX512.trunc_of_x
let extend_to_i512 = IX512.extend_to_x
let narrow_of_i512_opt = IX512.narrow_of_x_opt
let narrow_of_i512_hlt = IX512.narrow_of_x_hlt

module IU512 = Convert.Make_nbI_wU(T)(U512)
let trunc_of_u512 = IU512.trunc_of_u
let narrow_of_u512_opt = IU512.narrow_of_u_opt
let widen_to_u512_opt = IU512.widen_to_u_opt
let narrow_of_u512_hlt = IU512.narrow_of_u_hlt
let widen_to_u512_hlt = IU512.widen_to_u_hlt

module IX256 = Convert.Make_nbI_wX(T)(I256)
let trunc_of_i256 = IX256.trunc_of_x
let extend_to_i256 = IX256.extend_to_x
let narrow_of_i256_opt = IX256.narrow_of_x_opt
let narrow_of_i256_hlt = IX256.narrow_of_x_hlt

module IU256 = Convert.Make_nbI_wU(T)(U256)
let trunc_of_u256 = IU256.trunc_of_u
let narrow_of_u256_opt = IU256.narrow_of_u_opt
let widen_to_u256_opt = IU256.widen_to_u_opt
let narrow_of_u256_hlt = IU256.narrow_of_u_hlt
let widen_to_u256_hlt = IU256.widen_to_u_hlt

module IX128 = Convert.Make_nbI_wX(T)(I128)
let trunc_of_i128 = IX128.trunc_of_x
let extend_to_i128 = IX128.extend_to_x
let narrow_of_i128_opt = IX128.narrow_of_x_opt
let narrow_of_i128_hlt = IX128.narrow_of_x_hlt

module IU128 = Convert.Make_nbI_wU(T)(U128)
let trunc_of_u128 = IU128.trunc_of_u
let narrow_of_u128_opt = IU128.narrow_of_u_opt
let widen_to_u128_opt = IU128.widen_to_u_opt
let narrow_of_u128_hlt = IU128.narrow_of_u_hlt
let widen_to_u128_hlt = IU128.widen_to_u_hlt

include Convert.Make_nbI(T)

module IX32 = Convert.Make_nbI_nbX(T)(I32)
let trunc_of_i32 = IX32.trunc_of_x
let extend_to_i32 = IX32.extend_to_x
let narrow_of_i32_opt = IX32.narrow_of_x_opt
let narrow_of_i32_hlt = IX32.narrow_of_x_hlt

module IU32 = Convert.Make_nbI_nbU(T)(U32)
let trunc_of_u32 = IU32.trunc_of_u
let narrow_of_u32_opt = IU32.narrow_of_u_opt
let widen_to_u32_opt = IU32.widen_to_u_opt
let narrow_of_u32_hlt = IU32.narrow_of_u_hlt
let widen_to_u32_hlt = IU32.widen_to_u_hlt

module IX16 = Convert.Make_nbI_nbX(T)(I16)
let trunc_of_i16 = IX16.trunc_of_x
let extend_to_i16 = IX16.extend_to_x
let narrow_of_i16_opt = IX16.narrow_of_x_opt
let narrow_of_i16_hlt = IX16.narrow_of_x_hlt

module IU16 = Convert.Make_nbI_nbU(T)(U16)
let trunc_of_u16 = IU16.trunc_of_u
let narrow_of_u16_opt = IU16.narrow_of_u_opt
let widen_to_u16_opt = IU16.widen_to_u_opt
let narrow_of_u16_hlt = IU16.narrow_of_u_hlt
let widen_to_u16_hlt = IU16.widen_to_u_hlt
