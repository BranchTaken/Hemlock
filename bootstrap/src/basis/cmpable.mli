(** Functors for comparable types. *)

open Cmpable_intf

(** Comparisons for monomorphic types. *)
module Make (T : I_mono) : S_mono with type t := T.t

(** Comparisons for monomorphic types that include zero in their ranges. *)
module Make_zero (T : I_mono_zero) : S_mono_zero with type t := T.t

(** Comparisons for polymorphic types. *)
module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t
