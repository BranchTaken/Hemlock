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
  include Intw.MakeFI(U)
end
include T

module IZ = Convert.Make_wI_wZ(T)(Zint)
let trunc_of_zint = IZ.trunc_of_x
let extend_to_zint = IZ.extend_to_x
let narrow_of_zint_opt = IZ.narrow_of_x_opt
let narrow_of_zint_hlt = IZ.narrow_of_x_hlt

module IN = Convert.Make_wI_wN(T)(Nat)
let trunc_of_nat = IN.trunc_of_u
let narrow_of_nat_opt = IN.narrow_of_u_opt
let widen_to_nat_opt = IN.widen_to_u_opt
let narrow_of_nat_hlt = IN.narrow_of_u_hlt
let widen_to_nat_hlt = IN.widen_to_u_hlt
