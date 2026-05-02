open Basis
open! Basis.Rudiments

module T = struct
  module Index = Uns
  type t = {
    index: Index.t;
    lhs_index: SymbolIndex.t;
    rhs_indexes: SymbolIndex.t array;
    prec: Prec.t option;
    stmt: Parse.nonterm_prod option;
    callback: Callback.t;
  }

  let hash_fold {index; _} state =
    Uns.hash_fold index state

  let cmp {index=index0; _} {index=index1; _} =
    Index.cmp index0 index1

  let pp {index; lhs_index; rhs_indexes; prec; stmt; callback} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; lhs_index=" |> SymbolIndex.pp lhs_index
    |> Fmt.fmt "; rhs_indexes=" |> (Array.pp SymbolIndex.pp) rhs_indexes
    |> Fmt.fmt "; prec=" |> (Option.pp Prec.pp) prec
    |> Fmt.fmt "; stmt=" |> (Option.pp Parse.fmt_prod) stmt
    |> Fmt.fmt "; callback=" |> Callback.pp callback
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~index ~lhs_index ~rhs_indexes ~prec ~stmt ~callback =
  {index; lhs_index; rhs_indexes; prec; stmt; callback}

let is_synthetic {stmt; _} =
  Option.is_none stmt

let is_epsilon {rhs_indexes; _} =
  Array.is_empty rhs_indexes
