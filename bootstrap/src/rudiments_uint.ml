include Rudiments_uint0

module T = struct
  type t = uint
  let num_bits = uint_of_int Sys.int_size
end
include T
include Intnb.Make_u(T)
