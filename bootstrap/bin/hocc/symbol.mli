(** Grammar symbol. *)

open Basis
open! Basis.Rudiments

(** Declaration AST. *)
type stmt =
  | Token of Parse.token
  | Nonterm of Parse.nonterm

module Index = SymbolIndex
type t = {
  index: Index.t;
  (** Unique symbol index. *)

  name: string;
  (** Symbol name. *)

  qtype: QualifiedType.t;
  (** Qualified type, e.g. [Implicit] for [token SOME_TOKEN], or [Explicit {module_:Zint; type:t}]
      for [token INT of Zint.t. *)

  prec: Prec.t option;
  (** Optional precedence. *)

  stmt: stmt option;
  (** Optional declaration AST ([None] for synthetic symbols). *)

  alias: string option;
  (** Optional alias, e.g. [Some "+"] for [token PLUS "+"]. *)

  start: bool;
  (** True if start symbol. Always false for tokens. *)

  prods: (Prod.t, Prod.cmper_witness) Ordset.t;
  (** Productions associated with non-terminal. Always empty for tokens. *)

  first: (Index.t, Index.cmper_witness) Ordset.t;
  (** First set, i.e. the set of symbols which can begin a sequence rooted at this symbol. *)

  follow: (Index.t, Index.cmper_witness) Ordset.t;
  (** Follow set, i.e. the set of symbols which can immediately follow a sequence rooted at this
      symbol. *)
}

val epsilon: t
(** [epsilon] returns an epsilon (ε) symbol. *)

val pseudo_end: t
(** [pseudo_end] returns a pseudo-end (⊥) symbol. *)

val init_token: index:Index.t -> name:string -> qtype:QualifiedType.t -> prec:Prec.t option
  -> stmt:Parse.token option -> alias:string option -> t
(** Used only by [Symbols.insert_token]. *)

val init_nonterm: index:Index.t -> name:string -> qtype:QualifiedType.t -> prec:Prec.t option
  -> stmt:Parse.nonterm option -> start:bool -> prods:(Prod.t, Prod.cmper_witness) Ordset.t -> t
(** Used only by [Symbols.insert_nonterm]. *)

val is_token: t -> bool
(** [is_token t] returns true iff [t] is a token. *)

val is_nonterm: t -> bool
(** [is_token t] returns true iff [t] is a non-terminal. *)

val is_synthetic: t -> bool
(** [is_token t] returns true iff [t] is a synthetic symbol that has no concrete representation in
    the hocc grammar. *)

include IdentifiableIntf.S with type t := t

val pp_hr: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs symbol in human-readable form. *)

val index: t -> Index.t
(** [index t] returns the unique index associated with [t]. *)

val name: t -> string
(** [name t] returns the symbol name. *)

val first_mem: other:t -> t -> bool
(** [first_mem ~other t] returns true if [other] is in the first set of [t]. *)

val first_has_diff: (Index.t, Index.cmper_witness) Ordset.t -> t -> bool
(** [first_has_diff symbol_indexes t] returns true if [symbol_indexes] contains symbols not
    contained in the first set of [t]. *)

val first_insert: other:t -> t -> t
(** [first_insert ~other t] returns a symbol equivalent to [t] with [other] inserted into the first
    set. *)

val first_union: (Index.t, Index.cmper_witness) Ordset.t -> t -> t
(** [first_union symbol_indexes t] returns a symbol equivalent to [t] with all symbols in
    [symbol_indexes] inserted into the first set. *)

val follow_has_diff: (Index.t, Index.cmper_witness) Ordset.t -> t -> bool
(** [follow_has_diff symbol_indexes t] returns true if [symbol_indexes] contains symbols not
    contained in the follow set of [t]. *)

val follow_union: (Index.t, Index.cmper_witness) Ordset.t -> t -> t
(** [follow_union symbol_indexes t] returns a symbol equivalent to [t] with all symbols in
    [symbol_indexes] inserted into the follow set. *)
