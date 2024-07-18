(** Reduction callback code associated with a production. Conceptually a reduction callback is
    simply a block of code, but there is quite a bit of hair related to binding parameters to
    production symbols. *)

open Basis
open Basis.Rudiments

(** Reduction callback parameter. *)
module Param : sig
  type t = {
    binding: string option;
    (** Optional binding name for reduction callback code. Generated code must specify a binding for
        each RHS symbol it needs to access. *)

    symbol_name: string;
    (** Symbol name corresponding to a [start]/[nonterm] or [token] declaration. *)

    qtype: QualifiedType.t;
    (** Qualified type of parameter, e.g. [explicit_opt=Some {module_:"SomeToken"; type_:"t"}]. *)

    prod_param: Parse.nonterm_prod_param option;
    (** Declaration AST. *)
  }

  include IdentifiableIntf.S with type t := t

  val init: binding:string option -> symbol_name:string -> qtype:QualifiedType.t
    -> prod_param:Parse.nonterm_prod_param option -> t
end

(** Ordered container of reduction callback parameters. *)
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
  (** Unique reduction callback index. *)

  lhs_name: string;
  (** Name of enclosing nonterm. *)

  lhs_qtype: QualifiedType.t;
  (** Qualified type of LHS. *)

  rhs: Params.t;
  (** RHS parameters. *)

  code: Parse.nonterm_code option;
  (** Optional embedded code to be invoked by generated parser. *)
}

include IdentifiableIntf.S with type t := t

val init: index:Index.t -> lhs_name:string -> lhs_qtype:QualifiedType.t -> rhs:Params.t
  -> code:Parse.nonterm_code option -> t
(** Used only by [Callbacks.init]. *)

val is_epsilon: t -> bool
(** [is_epsilon t] returns true if [t] is an ε reduction callback, i.e. it has an empty RHS. *)
