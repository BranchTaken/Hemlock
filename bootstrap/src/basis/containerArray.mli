open ContainerArrayIntf

(** Seq-based [to_array] functor for monomorphic types, e.g. [string]. *)
module MakeMonoArray (T : IMonoArray) : SMonoArray
  with type t := T.t
  with type elm := T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyArray (T : IPolyArray) : SPolyArrayGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('a, 'cmp)
    Ordset]. *)
module MakePoly2Array (T : IPoly2Array) : SPoly2ArrayGen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
  with type 'a elm := 'a T.elm

(** Seq-based [to_array] functor for polymorphic types, e.g. [('k, 'v, 'cmp)
    Ordmap]. *)
module MakePoly3Array (T : IPoly3Array) : SPoly3ArrayGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value
