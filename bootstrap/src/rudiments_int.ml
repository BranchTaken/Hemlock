module T = struct
  type t = int
  let num_bits = Sys.int_size
end
include T
include Intnb.Make_i(T)
