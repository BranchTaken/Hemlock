open Basis
open Basis.Rudiments

module T = struct
  type t = u64 array
  let min_word_length = 0L
  let max_word_length = Uns.(2L ** 58L) (* 2**64 bits. *)
  let init n ~f =
    Array.init (0L =:< n) ~f
  let word_length = Array.length
  let get = Array.get
end
include T
include Intw.MakeVI(T)

let k_0 = of_uns 0x0L
let k_1 = of_uns 0x1L
let k_2 = of_uns 0x2L
let k_3 = of_uns 0x3L
let k_4 = of_uns 0x4L
let k_5 = of_uns 0x5L
let k_6 = of_uns 0x6L
let k_7 = of_uns 0x7L
let k_8 = of_uns 0x8L
let k_9 = of_uns 0x9L
let k_a = of_uns 0xaL
let k_b = of_uns 0xbL
let k_c = of_uns 0xcL
let k_d = of_uns 0xdL
let k_e = of_uns 0xeL
let k_f = of_uns 0xfL
let k_g = of_uns 0x10L
