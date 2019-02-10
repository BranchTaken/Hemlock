open Container_intf

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
