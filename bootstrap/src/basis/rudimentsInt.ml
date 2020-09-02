include RudimentsInt0

module T = struct
  type t = uns
  let bit_length = Sys.int_size
end
include T
include Intnb.MakeU(T)
