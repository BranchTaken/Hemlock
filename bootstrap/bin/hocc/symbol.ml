open Basis
open! Basis.Rudiments

module T = struct
  type stmt =
    | Token of Parse.nonterm_token
    | Nonterm of Parse.nonterm_nonterm

  let pp_stmt stmt formatter =
    match stmt with
    | Token token -> formatter |> Fmt.fmt "Token " |> Parse.fmt_token token
    | Nonterm nonterm -> formatter |> Fmt.fmt "Nonterm " |> Parse.fmt_nonterm nonterm

  module Index = SymbolIndex
  type t = {
    index: Index.t;
    name: string;
    stype: SymbolType.t;
    prec: Prec.t option;
    stmt: stmt option;
    alias: string option;
    start: bool;
    prods: (Prod.t, Prod.cmper_witness) Ordset.t;
    first: Bitset.t;
    follow: Bitset.t;
  }

  let hash_fold {index; _} state =
    state |> Index.hash_fold index

  let cmp {index=index0; _} {index=index1; _} =
    Index.cmp index0 index1

  let pp {index; name; stype; prec; stmt; alias; start; prods; first; follow} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; name=" |> String.pp name
    |> Fmt.fmt "; stype=" |> SymbolType.pp stype
    |> Fmt.fmt "; prec=" |> (Option.pp Prec.pp) prec
    |> Fmt.fmt "; stmt=" |> (Option.pp pp_stmt) stmt
    |> Fmt.fmt "; alias=" |> (Option.pp String.pp) alias
    |> Fmt.fmt "; start=" |> Bool.pp start
    |> Fmt.fmt "; prods=" |> Ordset.pp prods
    |> Fmt.fmt "; first=" |> Bitset.pp first
    |> Fmt.fmt "; follow=" |> Bitset.pp follow
    |> Fmt.fmt "}"

  let pp_hr {name; alias; prods; _} formatter =
    let pretty, pretty_name = match Ordset.is_empty prods, alias with
      | _, None
      | false, Some _ -> false, name
      | true, Some alias -> true, alias
    in
    formatter
    |> Fmt.fmt (String.to_string ~pretty pretty_name)
end
include T
include Identifiable.Make(T)

let init_token ~index ~name ~stype ~prec ~stmt ~alias =
  let stmt = match stmt with
    | None -> None
    | Some stmt -> Some (Token stmt)
  in
  let start = false in
  let prods = Ordset.empty (module Prod) in
  (* Tokens are in their own `first` sets. *)
  let first = Bitset.singleton index in
  let follow = Bitset.empty in
  {index; name; stype; prec; stmt; alias; start; prods; first; follow}

let init_synthetic_token ~index ~name ~alias =
  init_token ~index ~name ~stype:SymbolType.synthetic_implicit ~prec:None ~stmt:None
    ~alias:(Some alias)

let epsilon = init_synthetic_token ~index:0L ~name:"EPSILON" ~alias:"ε"

let pseudo_end = init_synthetic_token ~index:1L ~name:"PSEUDO_END" ~alias:"⊥"

let init_nonterm ~index ~name ~stype ~prec ~stmt ~start ~prods =
  let stmt = match stmt with
    | None -> None
    | Some stmt -> Some (Nonterm stmt)
  in
  let alias = None in
  (* Insert "ε" into the `first` set if there is an epsilon production. *)
  let has_epsilon_prod = Ordset.fold_until ~init:false ~f:(fun _has_epsilon_prod prod ->
    let is_epsilon = Prod.is_epsilon prod in
    is_epsilon, is_epsilon
  ) prods in
  let first = match has_epsilon_prod with
    | false -> Bitset.empty
    | true -> Bitset.singleton epsilon.index
  in
  (* Insert "ε" into the `follow` set for synthetic wrapper symbols. *)
  let follow = match stmt with
    | Some _ -> Bitset.empty
    | None -> Bitset.singleton epsilon.index
  in
  {index; name; stype; prec; stmt; alias; start; prods; first; follow}

let is_token {prods; _} =
  Ordset.is_empty prods

let is_nonterm t =
  not (is_token t)

let is_synthetic {stmt; _} =
  match stmt with
  | None -> true
  | Some _ -> false

let index {index; _} =
  index

let name {name; _} =
  name

let first_mem ~other t =
  Bitset.mem other.index t.first

let first_has_diff symbol_indexes t =
  not (Bitset.is_empty (Bitset.diff symbol_indexes t.first))

let first_insert ~other t =
  let first = Bitset.insert other.index t.first in
  {t with first}

let first_union symbol_indexes t =
  let first = Bitset.union symbol_indexes t.first in
  {t with first}

let follow_has_diff symbol_indexes t =
  not (Bitset.is_empty (Bitset.diff symbol_indexes t.follow))

let follow_union symbol_indexes t =
  let follow = Bitset.union symbol_indexes t.follow in
  {t with follow}
