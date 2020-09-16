include RudimentsInt0

module T = struct
  type t = uns
  let num_bits = Sys.int_size
end
include T
include Intnb.MakeU(T)
