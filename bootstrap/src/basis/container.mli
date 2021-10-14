(** Container functors. *)

open ContainerIntf

(** O(n) [length] functor for monomorphic types, e.g. [string]. *)
module MakeMonoLength (T : IMonoIter) : SMonoLength
  with type t := T.t
  with type elm := T.elm

(** Folding-related functor for monomorphic types, e.g. [string]. *)
module MakeMonoIter (T : IMonoIter) : SMonoIter
  with type t := T.t
  with type elm := T.elm

(** Seq-based [to_array] functor for monomorphic types, e.g. [string]. *)
module MakeMonoArray (T : IMonoIndex) : SMonoArray
  with type t := T.t
  with type elm := T.elm

(** General folding/length/membership functor for monomorphic container types, e.g. [string]. *)
module MakeMonoIndex (T : IMonoIndex) : SMonoIndex with type t := T.t with type elm := T.elm

(** O(n) [mem] functor for monomorphic types, e.g. [string]. *)
module MakeMonoMem (T : IMonoMem) : SMonoMem
  with type t := T.t
  with type elm := T.elm

(** O(n) [length] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyLength (T : IPolyIter) : SPolyLengthGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** Folding-related functor for polymorphic types, e.g. ['a list]. *)
module MakePolyIter (T : IPolyIter) : SPolyIterGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyArray (T : IPolyIndex) : SPolyArrayGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** General folding/length/membership functor for polymorphic container types, e.g. ['a list]. *)
module MakePolyIndex (T : IPolyIndex) : SPolyIndexGen
  with type 'a t := 'a T.t with type 'a elm := 'a T.elm

(** O(n) [mem] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyMem (T : IPolyMem) : SPolyMemGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('a, 'cmp) Ordset]. *)
module MakePoly2Index (T : IPoly2Index) : SPoly2IndexGen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
  with type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('k, 'v, 'cmp) Ordmap]. *)
module MakePoly3Index (T : IPoly3Index) : SPoly3IndexGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value
