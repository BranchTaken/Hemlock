(** Functors for container types. *)

open ContainerCommonIntf

(** O(n) [length] functor for monomorphic types, e.g. [string]. *)
module MakeMonoLength (T : IMono) : SMonoLength
  with type t := T.t
  with type elm := T.elm

(** Folding-related functor for monomorphic types, e.g. [string]. *)
module MakeMonoFold (T : IMono) : SMonoFold
  with type t := T.t
  with type elm := T.elm

(** O(n) [mem] functor for monomorphic types, e.g. [string]. *)
module MakeMonoMem (T : IMonoMem) : SMonoMem
  with type t := T.t
  with type elm := T.elm

(** O(n) [length] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyLength (T : IPoly) : SPolyLengthGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** Folding-related functor for polymorphic types, e.g. ['a list]. *)
module MakePolyFold (T : IPoly) : SPolyFoldGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(** O(n) [mem] functor for polymorphic types, e.g. ['a list]. *)
module MakePolyMem (T : IPolyMem) : SPolyMemGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm

(* For library-internal use. *)
module MakeIPoly (T : IMono) : IPoly
  with type 'a t = T.t
  with type 'a elm = T.elm
