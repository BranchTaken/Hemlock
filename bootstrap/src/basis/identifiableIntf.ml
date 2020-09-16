(** Identifiable type functor interface and signature. *)

(** Functor input interface for identifiable types. *)
module type I = sig
  type t
  include CmpableIntf.Key with type t := t
  include FormattableIntf.SMono with type t := t
end

(** Functor output signature for identifiable types. *)
module type S = sig
  include I
  include CmpableIntf.SMono with type t := t
  include Cmper.SMono with type t := t
end
