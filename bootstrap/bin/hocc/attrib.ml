open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    symbol_index: Symbol.Index.t;
    conflict: Contrib.t;
    ergo_lr1itemset: Lr1Itemset.t; (* Only the core matters for `hash_fold`/`cmp`/`equal`. *)
    contrib: Contrib.t;
  }

  let hash_fold {symbol_index; conflict; ergo_lr1itemset; contrib} state =
    state
    |> Uns.hash_fold 1L |> Symbol.Index.hash_fold symbol_index
    |> Uns.hash_fold 2L |> Contrib.hash_fold conflict
    |> Uns.hash_fold 2L |> Lr0Itemset.hash_fold (Lr1Itemset.core ergo_lr1itemset)
    |> Uns.hash_fold 3L |> Contrib.hash_fold contrib

  let cmp {symbol_index=s0; conflict=x0; ergo_lr1itemset=e0; contrib=c0}
    {symbol_index=s1; conflict=x1; ergo_lr1itemset=e1; contrib=c1} =
    let open Cmp in
    match Symbol.Index.cmp s0 s1 with
    | Lt -> Lt
    | Eq -> begin
        match Contrib.cmp x0 x1 with
        | Lt -> Lt
        | Eq -> begin
            match Lr0Itemset.cmp (Lr1Itemset.core e0) (Lr1Itemset.core e1) with
            | Lt -> Lt
            | Eq -> Contrib.cmp c0 c1
            | Gt -> Gt
          end
        | Gt -> Gt
      end
    | Gt -> Gt

  let equal {symbol_index=s0; conflict=x0; ergo_lr1itemset=e0; contrib=c0}
    {symbol_index=s1; conflict=x1; ergo_lr1itemset=e1; contrib=c1} =
    assert Symbol.Index.(s0 = s1);
    assert Contrib.(x0 = x1);
    Lr0Itemset.equal (Lr1Itemset.core e0) (Lr1Itemset.core e1) && Contrib.equal c0 c1

  let pp {symbol_index; conflict; ergo_lr1itemset; contrib} formatter =
    formatter
    |> Fmt.fmt "{symbol_index=" |> Symbol.Index.pp symbol_index
    |> Fmt.fmt "; conflict=" |> Contrib.pp conflict
    |> Fmt.fmt "; ergo_lr1itemset=" |> Lr1Itemset.pp ergo_lr1itemset
    |> Fmt.fmt "; contrib=" |> Contrib.pp contrib
    |> Fmt.fmt "}"

  let fmt_hr symbols prods ?(alt=false) ?(width=0L)
    {symbol_index; conflict; ergo_lr1itemset; contrib} formatter =
    formatter
    |> Fmt.fmt "{symbol_index="
    |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
    |> Fmt.fmt "; conflict="
    |> Contrib.pp_hr symbols prods conflict
    |> Fmt.fmt "; ergo_lr1itemset="
    |> Lr1Itemset.fmt_hr symbols ~alt ~width ergo_lr1itemset
    |> Fmt.fmt "; contrib="
    |> Contrib.pp_hr symbols prods contrib
    |> Fmt.fmt "}"

  let empty ~symbol_index ~conflict =
    {symbol_index; conflict; ergo_lr1itemset=Lr1Itemset.empty; contrib=Contrib.empty}

  let init ~symbol_index ~conflict ~ergo_lr1itemset ~contrib =
    {symbol_index; conflict; ergo_lr1itemset; contrib}

  let is_empty {contrib; _} =
    Contrib.is_empty contrib

  let union {symbol_index=s0; conflict=x0; ergo_lr1itemset=e0; contrib=c0}
    {symbol_index=s1; conflict=x1; ergo_lr1itemset=e1; contrib=c1} =
    assert Symbol.Index.(s0 = s1);
    assert Contrib.(x0 = x1);
    init ~symbol_index:s0 ~conflict:x0 ~ergo_lr1itemset:(Lr1Itemset.union e0 e1)
      ~contrib:(Contrib.union c0 c1)

  let inter {symbol_index=s0; conflict=x0; ergo_lr1itemset=e0; contrib=c0}
    {symbol_index=s1; conflict=x1; ergo_lr1itemset=e1; contrib=c1} =
    assert Symbol.Index.(s0 = s1);
    assert Contrib.(x0 = x1);
    init ~symbol_index:s0 ~conflict:x0 ~ergo_lr1itemset:(Lr1Itemset.inter e0 e1)
      ~contrib:(Contrib.inter c0 c1)
end
include T
include Identifiable.Make(T)
