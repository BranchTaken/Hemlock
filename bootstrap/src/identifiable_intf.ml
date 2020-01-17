(** Identifiable type functor interface and signature. *)

(** Functor input interface for identifiable types. *)
module type I = sig
  type t
  include Hashable_intf.Key with type t := t
  include Cmpable_intf.I_mono with type t := t
  include Formattable_intf.S_mono with type t := t
  include Stringable_intf.S with type t := t
end

(** Functor output signature for identifiable types. *)
module type S = sig
  include I
  include Cmpable_intf.S_mono with type t := t
end
