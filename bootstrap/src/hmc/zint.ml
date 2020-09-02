open Basis
open Basis.Rudiments

module T = struct
  type t = u64 array
  let min_word_length = 0
  let max_word_length = Uns.(2 ** 26) (* 2**32 bits. *)
  let init = Array.init
  let word_length = Array.length
  let get = Array.get
end
include T
include Intw.MakeVI(T)

let k_0 = of_uns 0x0
let k_1 = of_uns 0x1
let k_2 = of_uns 0x2
let k_3 = of_uns 0x3
let k_4 = of_uns 0x4
let k_5 = of_uns 0x5
let k_6 = of_uns 0x6
let k_7 = of_uns 0x7
let k_8 = of_uns 0x8
let k_9 = of_uns 0x9
let k_a = of_uns 0xa
let k_b = of_uns 0xb
let k_c = of_uns 0xc
let k_d = of_uns 0xd
let k_e = of_uns 0xe
let k_f = of_uns 0xf
let k_g = of_uns 0x10
