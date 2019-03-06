open Container_array_intf

(* Polymorphic, e.g. ('a array). *)

(* Seq-based to_array. *)
module Make_poly_array (T : I_poly_array) : S_poly_array_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* Monomorphic, e.g. string. *)

(* Seq-based to_array. *)
module Make_mono_array (T : I_mono_array) : S_mono_array with type t := T.t
                                                          and type elm := T.elm
