(** Functors for container types. *)

open Container_common_intf

(** O(n) [length] functor for monomorphic types, e.g. [string]. *)
module Make_mono_length (T : I_mono) : S_mono_length with type t := T.t
                                                      and type elm := T.elm

(** Folding-related functor for monomorphic types, e.g. [string]. *)
module Make_mono_fold (T : I_mono) : S_mono_fold with type t := T.t
                                                  and type elm := T.elm

(** O(n) [mem] functor for monomorphic types, e.g. [string]. *)
module Make_mono_mem (T : I_mono_mem) : S_mono_mem with type t := T.t
                                                    and type elm := T.elm

(** O(n) [length] functor for polymorphic types, e.g. ['a list]. *)
module Make_poly_length (T : I_poly) : S_poly_length_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(** Folding-related functor for polymorphic types, e.g. ['a list]. *)
module Make_poly_fold (T : I_poly) : S_poly_fold_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(** O(n) [mem] functor for polymorphic types, e.g. ['a list]. *)
module Make_poly_mem (T : I_poly_mem) : S_poly_mem_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(* For library-internal use. *)
module Make_i_poly (T : I_mono) : I_poly with type 'a t = T.t
                                          and type 'a elm = T.elm
