(** Reduction callback code associated with a production. Conceptually a reduction callback is
    simply a block of code, but there is quite a bit of hair related to binding parameters to
    production symbols. *)

open Basis
open Basis.Rudiments

(** Reduction callback parameter. *)
module Param : sig
  type t = {
    pattern: string option;
    (** Optional binding pattern for reduction callback code. Generated code must specify a binding
        for each RHS symbol it needs to access. *)

    symbol_name: string;
    (** Symbol name corresponding to a [start]/[nonterm] or [token] declaration. *)

    stype: SymbolType.t;
    (** Symbol type of parameter. *)

    prod_param: Parse.nonterm_prod_param option;
    (** Declaration AST. *)
  }

  include IdentifiableIntf.S with type t := t

  val init: pattern:string option -> symbol_name:string -> stype:SymbolType.t
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

  val bindings: t -> (string, String.cmper_witness) Set.t
  (** [bindings t] returns the set of binding identifier names it [t]. *)
end

module Index = Uns
type t = {
  index: Index.t;
  (** Unique reduction callback index. *)

  lhs_name: string;
  (** Name of enclosing nonterm. *)

  lhs_stype: SymbolType.t;
  (** Symbol type of LHS. *)

  rhs: Params.t;
  (** RHS parameters. *)

  code: Parse.nonterm_code option;
  (** Optional embedded code to be invoked by generated parser. *)
}

include IdentifiableIntf.S with type t := t

val init: index:Index.t -> lhs_name:string -> lhs_stype:SymbolType.t -> rhs:Params.t
  -> code:Parse.nonterm_code option -> t
(** Used only by [Callbacks.init]. *)

val is_epsilon: t -> bool
(** [is_epsilon t] returns true if [t] is an ε reduction callback, i.e. it has an empty RHS. *)
