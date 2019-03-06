open Container_intf

(* Polymorphic, e.g. ('a array). *)

(* O(n) length. *)
module Make_poly_length (T : I_poly) : S_poly_length
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* Folding-related functions. *)
module Make_poly_fold (T : I_poly) : S_poly_fold
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* O(n) mem. *)
module Make_poly_mem (T : I_poly_mem) : S_poly_mem
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* Seq-based to_array. *)
module Make_poly_array (T : I_poly_array) : S_poly_array
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* Monomorphic, e.g. string. *)

(* O(n) length. *)
module Make_mono_length (T : I_mono) : S_mono_length with type t := T.t
                                                    and type elm := T.elm

(* Folding-related functions. *)
module Make_mono_fold (T : I_mono) : S_mono_fold with type t := T.t
                                                  and type elm := T.elm

(* O(n) mem. *)
module Make_mono_mem (T : I_mono_mem) : S_mono_mem with type t := T.t
                                                    and type elm := T.elm

(* Seq-based to_array. *)
module Make_mono_array (T : I_mono_array) : S_mono_array with type t := T.t
                                                          and type elm := T.elm
