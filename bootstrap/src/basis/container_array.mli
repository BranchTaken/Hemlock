open Container_array_intf

(** Seq-based [to_array] functor for monomorphic types, e.g. [string]. *)
module Make_mono_array (T : I_mono_array) : S_mono_array with type t := T.t
                                                          and type elm := T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. ['a list]. *)
module Make_poly_array (T : I_poly_array) : S_poly_array_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('a, 'cmp)
    Ordset]. *)
module Make_poly2_array (T : I_poly2_array) : S_poly2_array_gen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
   and type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('k, 'v, 'cmp)
    Ordmap]. *)
module Make_poly3_array (T : I_poly3_array) : S_poly3_array_gen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
   and type 'k key := 'k T.key
   and type 'v value := 'v T.value
