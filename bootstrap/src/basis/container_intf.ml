(** Container functor signatures. *)

open Container_common_intf
open Container_array_intf

(** Polymorphic container, e.g. {!type:'a list}. *)
module type S_poly = sig
  include S_poly_length
  include S_poly_fold with type 'a t := 'a t
  include S_poly_mem with type 'a t := 'a t
  include S_poly_array with type 'a t := 'a t
end

(** Monomorphic container, e.g. {!type:string}. *)
module type S_mono = sig
  include S_mono_length
  include S_mono_fold with type t := t with type elm := elm
  include S_mono_mem with type t := t with type elm := elm
  include S_mono_array with type t := t with type elm := elm
end
