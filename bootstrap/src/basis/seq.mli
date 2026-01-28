(** Functors for sequences. *)

open SeqIntf

(** Functor for definite monomorphic sequences. *)
module MakeMonoDef (T : IMonoDef) : SMonoDef
  with type t := T.t
  with type elm := T.elm

(** Functor for indefinite monomorphic sequences. *)
module MakeMonoIndef (T : IMonoIndef) : SMonoIndef
  with type t := T.t
  with type elm := T.elm

(** Functor for coupled folding of monomorphic sequences. *)
module MakeMonoFold2 (T : IMonoFold2) : SMonoFold2
  with type t := T.container
  with type elm := T.elm

(** Functor for coupled folding of poly2 sequences. *)
module MakePoly2Fold2 (T : IPoly2Fold2) : SPoly2Fold2
  with type ('a, 'cmp) t := ('a, 'cmp) T.container
  with type 'a elm := 'a T.elm

(** Functor for coupled folding of poly2 sequences. *)
module MakePoly3Fold2 (T : IPoly3Fold2) : SPoly3Fold2
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.container
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value
