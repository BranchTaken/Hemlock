(** Container functor signatures. *)

open ContainerCommonIntf
open ContainerArrayIntf

(** Polymorphic container, e.g. {!type:'a list}. *)
module type SPoly = sig
  include SPolyLength
  include SPolyFold with type 'a t := 'a t
  include SPolyMem with type 'a t := 'a t
  include SPolyArray with type 'a t := 'a t
end

(** Monomorphic container, e.g. {!type:string}. *)
module type SMono = sig
  include SMonoLength
  include SMonoFold with type t := t with type elm := elm
  include SMonoMem with type t := t with type elm := elm
  include SMonoArray with type t := t with type elm := elm
end
