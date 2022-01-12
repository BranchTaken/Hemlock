open RudimentsInt0

module T = struct
  module U = struct
    type t = u128
    let word_length = 2L

    let init ~f =
      u128_init ~f

    let get = u128_get
  end
  include U
  include Intw.MakeFU(U)
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

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

module UX256 = Convert.Make_wU_wX(T)(I256)
let trunc_of_i256 = UX256.trunc_of_x
let extend_to_i256 = UX256.extend_to_x
let narrow_of_i256_opt = UX256.narrow_of_x_opt
let narrow_of_i256_hlt = UX256.narrow_of_x_hlt

module UU256 = Convert.Make_wU_wU(T)(U256)
let trunc_of_u256 = UU256.trunc_of_u
let extend_to_u256 = UU256.extend_to_u
let narrow_of_u256_opt = UU256.narrow_of_u_opt
let narrow_of_u256_hlt = UU256.narrow_of_u_hlt

module UI128 = Convert.Make_wU_wI(T)(I128)
let bits_of_i128 = UI128.bits_of_x
let bits_to_i128 = UI128.bits_to_x
let like_of_i128_opt = UI128.like_of_x_opt
let like_to_i128_opt = UI128.like_to_x_opt
let like_of_i128_hlt = UI128.like_of_x_hlt
let like_to_i128_hlt = UI128.like_to_x_hlt
