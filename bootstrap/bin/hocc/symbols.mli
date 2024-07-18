(** Collection of all symbols, with automatic assignment of unique indexes. *)

open Basis
open! Basis.Rudiments

(* Ephemeral symbol information. Symbols have to be processed in two passes due to their mutually
 * recursive form. `info` captures only the name->metadata required of the first pass. *)
type info = {
  index: Symbol.Index.t;
  (** Unique symbol index. *)

  name: string;
  (** Symbol name. *)

  alias: string option;
  (** Optional token alias. *)

  qtype: QualifiedType.t;
  (** Qualified type, e.g. [explicit_opt=None] for [token SOME_TOKEN], or [explicit_opt=Some
      {module_:Zint; type:t}] for [token INT of Zint.t. *)
}

type t

val empty: t
(** [empty] returns an empty set of symbols. *)

val insert_token: name:string -> qtype:QualifiedType.t -> prec:Prec.t option
  -> stmt:Parse.nonterm_token option -> alias:string option -> t -> t
(** [insert_token ~name ~qtype ~prec ~stmt ~alias t] creates a token [Symbol.t] with unique index
    and returns a new [t] with the symbol inserted. *)

val insert_nonterm_info: name:string -> qtype:QualifiedType.t -> t -> t
(** [insert_nonterm_info ~name ~qtype t] creates a non-terminal [info] and returns a new [t] with
    the info inserted. This is a precursor to a subsequent [insert_nonterm] call. *)

val insert_nonterm: name:string -> prec:Prec.t option -> stmt:Parse.nonterm_nonterm option
  -> start:bool -> prods:(Prod.t, Prod.cmper_witness) Ordset.t -> t -> t
(** [insert_token ~name ~prec ~stmt ~start ~prods t] creates a non-terminal [Symbol.t] with unique
    index and returns a new [t] with the symbol inserted. *)

val update_symbol: Symbol.t -> t -> t
(** [update_symbol symbol t] returns a new [t] containing [symbol] rather than an incremental
    precursor of [symbol]. This function is used when incrementally computing symbols' first and
    follow sets. *)

val info_of_name: string -> t -> info option
(** [info_of_name name t] returns [Some info] if a symbol with the specified [name] exists, [None]
    otherwise. *)

val info_of_name_hlt: string -> t -> info
(** [info_of_name name t] returns [Some info] if a symbol with the specified [name] exists, halts
    otherwise. *)

val info_of_alias: string -> t -> info option
(** [info_of_alias alias t] returns [Some info] if a symbol with the specified [alias] exists,
    [None] otherwise. Note that names and aliases are in separate namespaces. *)

val info_of_alias_hlt: string -> t -> info
(** [info_of_alias alias t] returns [Some info] if a symbol with the specified [alias] exists, halts
    otherwise. Note that names and aliases are in separate namespaces. *)

val symbol_index_of_name: string -> t -> Symbol.Index.t option
(** [symbol_index_of_name name t] returns [Some index] if a symbol with the specified [name] exists,
    [None] otherwise. *)

val symbol_of_name: string -> t -> Symbol.t option
(** [symbol_index_of_name name t] returns [Some index] if a symbol with the specified [name] exists,
    halts otherwise. *)

val symbol_index_of_alias: string -> t -> Symbol.Index.t option
(** [symbol_index_of_alias alias t] returns [Some index] if a symbol with the specified [alias]
    exists, [None] otherwise. *)

val symbol_of_alias: string -> t -> Symbol.t option
(** [symbol_index_of_alias alias t] returns [Some index] if a symbol with the specified [alias]
    exists, halts otherwise. *)

val symbol_of_symbol_index: Symbol.Index.t -> t -> Symbol.t
(** [symbol_of_symbol_index index t] returns [Some symbol] if a symbol with the specified [index]
    exists, halts otherwise. *)

val symbols_length: t -> uns
(** [symbols_length t] returns the number of symbols in [t]. *)

val tokens_length: t -> uns
(** [tokens_length t] returns the number of tokens in [t]. *)

val nonterms_length: t -> uns
(** [nonterms_length t] returns the number of non-terminals in [t]. *)

val symbols_fold: init:'accum -> f:('accum -> Symbol.t -> 'accum) -> t -> 'accum
(** [symbols_fold ~init ~f t] iteratively applies [f] to the symbols in [t], in increasing index
    order. *)

val tokens_fold: init:'accum -> f:('accum -> Symbol.t -> 'accum) -> t -> 'accum
(** [tokens_fold ~init ~f t] iteratively applies [f] to the tokens in [t], in increasing index
    order. *)

val nonterms_fold: init:'accum -> f:('accum -> Symbol.t -> 'accum) -> t -> 'accum
(** [nonterms_fold ~init ~f t] iteratively applies [f] to the non-terminals in [t], in increasing
    index order. *)

val src_fmt: Symbol.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs symbol in hocc syntax. *)

val pp_prod_hr: Prod.t -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs production in human-readable form. *)
