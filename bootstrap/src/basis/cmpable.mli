(** Functors for comparable types. *)

open CmpableIntf

(** Comparisons for monomorphic types. *)
module Make (T : IMono) : SMono with type t := T.t

(** Comparisons for monomorphic types that include zero in their ranges. *)
module MakeZero (T : IMonoZero) : SMonoZero with type t := T.t

(** Comparisons for polymorphic types. *)
module MakePoly (T : IPoly) : SPoly with type 'a t := 'a T.t

(** Comparisons for polymorphic types. *)
module MakePoly2 (T : IPoly2) : SPoly2
  with type ('a, 'cmp) t := ('a, 'cmp) T.t

(** Comparisons for polymorphic types. *)
module MakePoly3 (T : IPoly3) : SPoly3
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
