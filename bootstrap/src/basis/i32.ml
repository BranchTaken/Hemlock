(* Partial Rudiments. *)
open RudimentsInt0

module T = struct
  module U = struct
    type t = sint
    let bit_length = 32L
  end
  include U
  include Intnb.MakeI(U)
end
include T

module IZ = Convert.Make_nbI_wZ(T)(Zint)
let trunc_of_zint = IZ.trunc_of_x
let extend_to_zint = IZ.extend_to_x
let narrow_of_zint_opt = IZ.narrow_of_x_opt
let narrow_of_zint_hlt = IZ.narrow_of_x_hlt

module IN = Convert.Make_nbI_wN(T)(Nat)
let trunc_of_nat = IN.trunc_of_u
let narrow_of_nat_opt = IN.narrow_of_u_opt
let widen_to_nat_opt = IN.widen_to_u_opt
let narrow_of_nat_hlt = IN.narrow_of_u_hlt
let widen_to_nat_hlt = IN.widen_to_u_hlt

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
