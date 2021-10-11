(** Container functor signatures. *)

open ContainerCommonIntf
open ContainerArrayIntf

(** General functor input interface for monomorphic containers, e.g. {!type:string}. *)
module type IMono = sig
  include IMonoCommon
  include IMonoMem with type t := t with type elm := elm
  include IMonoArray with type t := t with type elm := elm
end

(** General functor input interface for polymorphic containers, e.g. {!type:'a list}. *)
module type IPoly = sig
  include IPolyCommon
  include IPolyMem with type 'a t := 'a t with type 'a elm := 'a elm
  include IPolyArray with type 'a t := 'a t with type 'a elm := 'a elm
end

(** Monomorphic container, e.g. {!type:string}. *)
module type SMono = sig
  include SMonoLength
  include SMonoFold with type t := t with type elm := elm
  include SMonoMem with type t := t with type elm := elm
  include SMonoArray with type t := t with type elm := elm
end

(** Polymorphic container, e.g. {!type:'a list}. *)
module type SPoly = sig
  include SPolyLength
  include SPolyFold with type 'a t := 'a t
  include SPolyMem with type 'a t := 'a t
  include SPolyArray with type 'a t := 'a t
end

(** {!module:SPolyGen} is equivalent to {!module:SPoly}, except that {!type:'a elm} is explicit.
    This near-identical signature exists exclusively to enable functor implementation. *)
module type SPolyGen = sig
  include SPolyLengthGen
  include SPolyFoldGen with type 'a t := 'a t with type 'a elm := 'a elm
  include SPolyMemGen with type 'a t := 'a t with type 'a elm := 'a elm
  include SPolyArrayGen with type 'a t := 'a t with type 'a elm := 'a elm
end
