open RudimentsInt

module T = struct
  module U = struct
    type t = u128
    let word_length = 2

    let init ~f =
      u128_init ~f

    let get = u128_get
  end
  include U
  include Intw.MakeFU(U)
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

let to_tup = u128_to_tup
