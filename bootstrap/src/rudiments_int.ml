open Rudiments_uint

module T = struct
  type t = int
  let num_bits = uint_of_int Sys.int_size
end
include T
include Intnb.Make_i(T)
