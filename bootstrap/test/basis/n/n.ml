open! Basis.Rudiments
open! Basis

module T = struct
  type t = u64 array
  let min_word_length = 0L
  let max_word_length = Uns.(2L ** 26L) (* 2**32 bits. *)
  let init = Array.init
  let word_length = Array.length
  let get = Array.get
end
include T
include Intw.MakeVU(T)
