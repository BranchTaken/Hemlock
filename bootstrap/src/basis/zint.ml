module T = struct
  type t = int64 Stdlib.Array.t
  let min_word_length = 0L
  let max_word_length = 0x400_0000_0000_0000L (* 2**64 bits. *)
  let init n ~f =
    Stdlib.Array.init (Stdlib.Int64.to_int n) (fun i -> f (Stdlib.Int64.of_int i))
  let word_length t =
    Stdlib.Int64.of_int (Stdlib.Array.length t)
  let get i t =
    Stdlib.Array.get t (Stdlib.Int64.to_int i)
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
