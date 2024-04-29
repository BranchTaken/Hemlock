open Basis
open! Basis.Rudiments

module K = struct
  module T = struct
    type t = {
      symbol_index: Symbol.Index.t;
      conflict: Contrib.t;
    }

    let hash_fold {symbol_index; _} state =
      state |> Symbol.Index.hash_fold symbol_index

    let cmp {symbol_index=s0; _} {symbol_index=s1; _} =
      Symbol.Index.cmp s0 s1

    let pp {symbol_index; conflict} formatter =
      formatter
      |> Fmt.fmt "{symbol_index=" |> Symbol.Index.pp symbol_index
      |> Fmt.fmt "; conflict=" |> Contrib.pp conflict
      |> Fmt.fmt "}"

    let pp_hr symbols prods {symbol_index; conflict} formatter =
      formatter
      |> Fmt.fmt "{symbol_index="
      |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
      |> Fmt.fmt "; conflict=" |> Contrib.pp_hr symbols prods conflict
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)

  let init ~symbol_index ~conflict =
    {symbol_index; conflict}
end

module V = struct
  module T = struct
    type t = {
      ergo_lr1itemset: Lr1Itemset.t; (* Only the core matters for `hash_fold`/`cmp`/`equal`. *)
      contrib: Contrib.t;
    }

    let hash_fold {ergo_lr1itemset; contrib} state =
      state
      |> Uns.hash_fold 0L |> Lr0Itemset.hash_fold (Lr1Itemset.core ergo_lr1itemset)
      |> Uns.hash_fold 1L |> Contrib.hash_fold contrib

    let cmp {ergo_lr1itemset=e0; contrib=c0} {ergo_lr1itemset=e1; contrib=c1} =
      let open Cmp in
      match Lr0Itemset.cmp (Lr1Itemset.core e0) (Lr1Itemset.core e1) with
      | Lt -> Lt
      | Eq -> Contrib.cmp c0 c1
      | Gt -> Gt

    let equal {ergo_lr1itemset=e0; contrib=c0} {ergo_lr1itemset=e1; contrib=c1} =
      Lr0Itemset.equal (Lr1Itemset.core e0) (Lr1Itemset.core e1) && Contrib.equal c0 c1

    let pp {ergo_lr1itemset; contrib} formatter =
      formatter
      |> Fmt.fmt "{ergo_lr1itemset=" |> Lr1Itemset.pp ergo_lr1itemset
      |> Fmt.fmt "; contrib=" |> Contrib.pp contrib
      |> Fmt.fmt "}"

    let fmt_hr symbols prods ?(alt=false) ?(width=0L) {ergo_lr1itemset; contrib} formatter =
      formatter
      |> Fmt.fmt "{ergo_lr1itemset="
      |> Lr1Itemset.fmt_hr symbols ~alt ~width ergo_lr1itemset
      |> Fmt.fmt "; contrib="
      |> Contrib.pp_hr symbols prods contrib
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)

  let empty = {
    ergo_lr1itemset=Lr1Itemset.empty;
    contrib=Contrib.empty;
  }

  let init ~ergo_lr1itemset ~contrib =
    {ergo_lr1itemset; contrib}

  let is_empty {contrib; _} =
    Contrib.is_empty contrib

  let merge {ergo_lr1itemset=e0; contrib=c0} {ergo_lr1itemset=e1; contrib=c1} =
    {ergo_lr1itemset=Lr1Itemset.union e0 e1; contrib=Contrib.union c0 c1}

  let union {ergo_lr1itemset=e0; contrib=c0} {ergo_lr1itemset=e1; contrib=c1} =
    {ergo_lr1itemset=Lr1Itemset.union e0 e1; contrib=Contrib.union c0 c1}

  let inter {ergo_lr1itemset=e0; contrib=c0} {ergo_lr1itemset=e1; contrib=c1} =
    {ergo_lr1itemset=Lr1Itemset.inter e0 e1; contrib=Contrib.inter c0 c1}
end

module T = struct
  type t = {
    k: K.t;
    v: V.t;
  }

  let hash_fold {k; v} state =
    state
    |> K.hash_fold k
    |> V.hash_fold v

  let cmp {k=k0; v=v0} {k=k1; v=v1} =
    let open Cmp in
    match K.cmp k0 k1 with
    | Lt -> Lt
    | Eq -> V.cmp v0 v1
    | Gt -> Gt

  let pp {k; v} formatter =
    formatter
    |> Fmt.fmt "{k="
    |> K.pp k
    |> Fmt.fmt "; v="
    |> V.pp v
    |> Fmt.fmt "}"

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) {k; v} formatter =
    formatter
    |> Fmt.fmt "{k="
    |> K.pp_hr symbols prods k
    |> Fmt.fmt "; v="
    |> V.fmt_hr symbols prods ~alt ~width v
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)
