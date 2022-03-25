open Basis
open! Basis.Rudiments

module Akey = struct
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

module Aval = struct
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
  type t = (Akey.t, Akey.t * Aval.t, Akey.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold (fun (_akey, aval) state -> state |> Aval.hash_fold aval)

  let cmp = Ordmap.cmp (fun (_akey0, aval0) (_akey1, aval1) -> Aval.cmp aval0 aval1)

  let fmt ?(alt=false) ?(width=0L) t formatter =
    formatter |> Ordmap.fmt ~alt ~width (fun (_akey, aval) formatter -> formatter |> Aval.pp aval) t

  let pp = Ordmap.pp (fun (_akey, aval) formatter -> formatter |> Aval.pp aval)

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    formatter
    |> (fun formatter ->
      List.fmt ~alt ~width (fun (symbol, aval) formatter ->
        formatter
        |> Akey.pp_hr symbols prods symbol
        |> Fmt.fmt " = "
        |> (fun (_akey, aval) formatter ->
          formatter |> Aval.fmt_hr symbols prods ~alt ~width:(width + 4L) aval
        ) aval
      ) (Ordmap.to_alist t) formatter
    )
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal (fun (_akey0, aval0) (_akey1, aval1) -> Aval.equal aval0 aval1) t0 t1

module Seq = struct
  type container = t
  type elm = Akey.t * (Akey.t * Aval.t)
  type t = (Akey.t, Akey.t * Aval.t, Akey.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Akey)

let singleton akey aval =
  Ordmap.singleton (module Akey) ~k:akey ~v:(akey, aval)

let is_empty = Ordmap.is_empty

let get symbol_index t =
  let akey = Akey.init ~symbol_index ~conflict:Contrib.empty in
  Ordmap.get akey t

let amend akey ~f t =
  Ordmap.amend akey ~f:(fun akey_aval_option ->
    let aval_opt = match akey_aval_option with
      | None -> f None
      | Some (_akey, aval) -> f (Some aval)
    in
    match aval_opt with
    | None -> None
    | Some aval -> Some (akey, aval)
  ) t

let insert akey aval t =
  Ordmap.amend akey ~f:(function
    | None -> Some (akey, aval)
    | Some (akey_prev, aval_prev) -> begin
        assert Contrib.(Akey.(akey.conflict) = Akey.(akey_prev.conflict));
        Some (akey_prev, Aval.union aval aval_prev)
      end
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _akey (akey0, aval0) (akey1, aval1) ->
    assert Contrib.(Akey.(akey0.conflict) = Akey.(akey1.conflict));
    akey0, Aval.union aval0 aval1
  ) t0 t1

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_akey, akey_aval) -> f accum akey_aval) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_akey, akey_aval) -> f accum akey_aval) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (_akey, akey_aval) -> f akey_aval) t

let fold2_until ~init ~f t =
  Ordmap.fold2_until ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    f accum kv_opt0 kv_opt1
  ) t

let fold2 ~init ~f t =
  Ordmap.fold2 ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    f accum kv_opt0 kv_opt1
  ) t

let symbol_indexes t =
  fold ~init:(Ordset.empty (module Symbol.Index))
    ~f:(fun symbol_indexes (Akey.{symbol_index; _}, _aval) ->
      Ordset.insert symbol_index symbol_indexes
    ) t
