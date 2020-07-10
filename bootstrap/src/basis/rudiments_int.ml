include Rudiments_int0

module T = struct
  type t = uns
  let num_bits = Sys.int_size
end
include T
include Intnb.Make_u(T)
