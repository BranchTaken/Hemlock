(** Functors for sequences. *)

open Seq_intf

(** Functor for definite monomorphic sequences. *)
module Make_def (T : I_mono_def) : S_mono_def with type t := T.t
                                              with type elm := T.elm

(** Functor for indefinite monomorphic sequences. *)
module Make_indef (T : I_mono_indef) : S_mono_indef with type t := T.t
                                                    with type elm := T.elm

(** Functor for coupled folding of poly2 sequences. *)
module Make_poly2_fold2 (T : I_poly2_fold2) : S_poly2_fold2
  with type ('a, 'cmp) t := ('a, 'cmp) T.container
  with type 'a elm := 'a T.elm

(** Functor for coupled folding of poly2 sequences. *)
module Make_poly3_fold2 (T : I_poly3_fold2) : S_poly3_fold2
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.container
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value
