module type I = sig
  type t
  include Hashable_intf.Key with type t := t
  include Cmpable_intf.I with type t := t
  include Sexpable_intf.S with type t := t
  include Stringable_intf.S with type t := t
end

module type S = sig
  include I
  include Cmpable_intf.S with type t := t
end
