(** Reduction code associated with a production. Conceptually a reduction is simply a block of code,
    but there is quite a bit of hair related to binding parameters to production symbols. *)

open Basis
open Basis.Rudiments

(** Reduction parameter. *)
module Param : sig
  type t = {
    binding: string option;
    (** Optional binding name for reduction code. Generated code must specify a binding for each RHS
        symbol it needs to access. *)

    symbol_name: string;
    (** Symbol name corresponding to a [start]/[nonterm] or [token] declaration. *)

    qtype: QualifiedType.t;
    (** Qualified type of parameter, e.g. [Explicit {module_:"SomeToken"; type_:"t"}]. *)

    prod_param: Parse.prod_param option;
    (** Declaration AST. *)
  }

  include IdentifiableIntf.S with type t := t

  val init: binding:string option -> symbol_name:string -> qtype:QualifiedType.t
    -> prod_param:Parse.prod_param option -> t
end

(** Ordered container of reduction parameters. *)
module Params : sig
  type t
  type elm = Param.t

  include IdentifiableIntf.S with type t := t
  include ContainerIntf.SMonoArray with type t := t with type elm := elm
  include ContainerIntf.SMonoIndex with type t := t with type elm := elm

  val init: Io.t -> Param.t array -> Io.t * t
  val length: t -> uns
  val range: t -> range
  val get: uns -> t -> Param.t
  val map: f:(Param.t -> 'a) -> t -> 'a array
end

module Index = Uns
type t = {
  index: Index.t;
  (** Unique reduction index. *)

  lhs: QualifiedType.t;
  (** Qualified type of LHS. *)

  rhs: Params.t;
  (** RHS parameters. *)

  code: Parse.code option;
  (** Optional embedded code to be invoked by generated parser. *)
}

include IdentifiableIntf.S with type t := t

val init: index:Index.t -> lhs:QualifiedType.t -> rhs:Params.t -> code:Parse.code option -> t
(** Used only by [Reductions.init]. *)
