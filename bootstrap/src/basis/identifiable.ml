open Identifiable_intf

module Make (T : I) : S with type t := T.t = struct
  include T
  include Cmpable.Make(T)
  include Cmper.Make_mono(T)
end
