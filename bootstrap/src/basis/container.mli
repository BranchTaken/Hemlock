open ContainerIntf

(** General folding/length/membership functor for monomorphic container types, e.g. [string]. *)
module MakeMono (T : IMono) : SMono with type t := T.t with type elm := T.elm

(** General folding/length/membership functor for polymorphic container types, e.g. ['a list]. *)
module MakePoly (T : IPoly) : SPolyGen with type 'a t := 'a T.t with type 'a elm := 'a T.elm
