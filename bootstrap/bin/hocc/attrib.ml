open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    conflict_state_index: StateIndex.t;
    symbol_index: Symbol.Index.t;
    conflict: Contrib.t;
    isucc_lr1itemset: Lr1Itemset.t; (* Only the core matters for `hash_fold`/`cmp`/`equal`. *)
    contrib: Contrib.t;
  }

  let hash_fold {conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib} state =
    state
    |> Uns.hash_fold 1L |> StateIndex.hash_fold conflict_state_index
    |> Uns.hash_fold 2L |> Symbol.Index.hash_fold symbol_index
    |> Uns.hash_fold 3L |> Contrib.hash_fold conflict
    |> Uns.hash_fold 4L |> Lr0Itemset.hash_fold (Lr1Itemset.core isucc_lr1itemset)
    |> Uns.hash_fold 5L |> Contrib.hash_fold contrib

  let cmp
      {conflict_state_index=csi0; symbol_index=s0; conflict=x0; isucc_lr1itemset=is0; contrib=c0}
      {conflict_state_index=csi1; symbol_index=s1; conflict=x1; isucc_lr1itemset=is1; contrib=c1} =
    let open Cmp in
    match StateIndex.cmp csi0 csi1 with
    | Lt -> Lt
    | Eq -> begin
        match Symbol.Index.cmp s0 s1 with
        | Lt -> Lt
        | Eq -> begin
            match Contrib.cmp x0 x1 with
            | Lt -> Lt
            | Eq -> begin
                match Lr0Itemset.cmp (Lr1Itemset.core is0) (Lr1Itemset.core is1) with
                | Lt -> Lt
                | Eq -> Contrib.cmp c0 c1
                | Gt -> Gt
              end
            | Gt -> Gt
          end
        | Gt -> Gt
      end
    | Gt -> Gt

  let equal_keys
      {conflict_state_index=csi0; symbol_index=s0; conflict=x0; _}
      {conflict_state_index=csi1; symbol_index=s1; conflict=x1; _} =
    StateIndex.(csi0 = csi1) &&
    Symbol.Index.(s0 = s1) &&
    Contrib.(x0 = x1)

  let remergeable_keys
      {conflict_state_index=csi0; symbol_index=s0; _}
      {conflict_state_index=csi1; symbol_index=s1; _} =
    StateIndex.(csi0 = csi1) &&
    Symbol.Index.(s0 = s1)

  let equal
      ({isucc_lr1itemset=is0; contrib=c0; _} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equal_keys t0 t1);
    Lr0Itemset.equal (Lr1Itemset.core is0) (Lr1Itemset.core is1) && Contrib.equal c0 c1

  let pp {conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib} formatter =
    formatter
    |> Fmt.fmt "{conflict_state_index=" |> StateIndex.pp conflict_state_index
    |> Fmt.fmt "; symbol_index=" |> Symbol.Index.pp symbol_index
    |> Fmt.fmt "; conflict=" |> Contrib.pp conflict
    |> Fmt.fmt "; isucc_lr1itemset=" |> Lr1Itemset.pp isucc_lr1itemset
    |> Fmt.fmt "; contrib=" |> Contrib.pp contrib
    |> Fmt.fmt "}"

  let fmt_hr symbols prods ?(alt=false) ?(width=0L)
    {conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib} formatter =
    formatter
    |> Fmt.fmt "{conflict_state_index="
    |> StateIndex.pp conflict_state_index
    |> Fmt.fmt "; symbol_index="
    |> Symbol.Index.pp symbol_index
    |> Fmt.fmt " (" |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
    |> Fmt.fmt "); conflict="
    |> Contrib.pp_hr symbols prods conflict
    |> Fmt.fmt "; isucc_lr1itemset="
    |> Lr1Itemset.fmt_hr symbols ~alt ~width isucc_lr1itemset
    |> Fmt.fmt "; contrib="
    |> Contrib.pp_hr symbols prods contrib
    |> Fmt.fmt "}"

  let init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset ~contrib =
    {conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib}

  let remerge1 remergeable_index_map ({conflict_state_index; _} as t) =
    let conflict_state_index' = match Ordmap.get conflict_state_index remergeable_index_map with
      | None -> conflict_state_index
      | Some conflict_state_index' -> conflict_state_index'
    in
    {t with conflict_state_index=conflict_state_index'}

  let is_empty {isucc_lr1itemset; contrib; _} =
    Lr1Itemset.is_empty isucc_lr1itemset &&
    Contrib.is_empty contrib

  let union_impl equalish_keys
      ({isucc_lr1itemset=is0; contrib=c0; _} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equalish_keys t0 t1);
    {t0 with isucc_lr1itemset=(Lr1Itemset.union is0 is1); contrib=(Contrib.union c0 c1)}

  let union t0 t1 =
    union_impl equal_keys t0 t1

  let union_remerged t0 t1 =
    union_impl remergeable_keys t0 t1

  let inter
      ({isucc_lr1itemset=is0; contrib=c0; _} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equal_keys t0 t1);
    {t0 with isucc_lr1itemset=(Lr1Itemset.inter is0 is1); contrib=(Contrib.inter c0 c1)}

  let diff
      ({isucc_lr1itemset=is0; contrib=c0; _} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equal_keys t0 t1);
    {t0 with isucc_lr1itemset=Lr1Itemset.diff is0 is1; contrib=Contrib.diff c0 c1}
end
include T
include Identifiable.Make(T)

let contrib {conflict; contrib; _} =
  (* Merge shift into contribs if present in the conflict manifestation, since all lanes are
   * implicated in shift actions. *)
  match Contrib.mem_shift conflict with
  | false -> contrib
  | true -> Contrib.(union shift contrib)

let compat_ielr ~resolve symbols prods ({symbol_index; _} as t0) t1 =
  (* Attribs that contribute nothing to the conflict are oblivious to merging. Otherwise resolution
   * must be equal for attribs to be compatible. *)
  let c0 = contrib t0 in
  match Contrib.is_empty c0 with
  | true -> true
  | false -> begin
      let c1 = contrib t1 in
      match Contrib.is_empty c1 with
      | true -> true
      | false -> begin
          match resolve with
          | false -> Contrib.equal c0 c1
          | true -> begin
              let r0 = Contrib.resolve symbols prods symbol_index c0 in
              let r1 = Contrib.resolve symbols prods symbol_index c1 in
              Contrib.equal r0 r1
            end
        end
    end
