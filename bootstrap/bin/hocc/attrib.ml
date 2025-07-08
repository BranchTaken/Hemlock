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

  let empty ~conflict_state_index ~symbol_index ~conflict =
    {conflict_state_index; symbol_index; conflict; isucc_lr1itemset=Lr1Itemset.empty;
      contrib=Contrib.empty}

  let init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset ~contrib =
    {conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib}

  let remerge1 remergeable_index_map ({conflict_state_index; _} as t) =
    let conflict_state_index' = match Ordmap.get conflict_state_index remergeable_index_map with
      | None -> conflict_state_index
      | Some conflict_state_index' -> conflict_state_index'
    in
    {t with conflict_state_index=conflict_state_index'}

  let reindex index_map ({conflict_state_index; _} as t) =
    match Ordmap.get conflict_state_index index_map with
    | None -> None
    | Some conflict_state_index' -> Some {t with conflict_state_index=conflict_state_index'}

  let is_empty {isucc_lr1itemset; contrib; _} =
    Lr1Itemset.is_empty isucc_lr1itemset &&
    Contrib.is_empty contrib

  let union_impl equalish_keys
      ({conflict_state_index; symbol_index; conflict; isucc_lr1itemset=is0; contrib=c0} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equalish_keys t0 t1);
    init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset:(Lr1Itemset.union is0 is1)
      ~contrib:(Contrib.union c0 c1)

  let union t0 t1 =
    union_impl equal_keys t0 t1

  let union_remerged t0 t1 =
    union_impl remergeable_keys t0 t1

  let inter
      ({conflict_state_index; symbol_index; conflict; isucc_lr1itemset=is0; contrib=c0} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equal_keys t0 t1);
    init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset:(Lr1Itemset.inter is0 is1)
      ~contrib:(Contrib.inter c0 c1)

  let diff
      ({conflict_state_index; symbol_index; conflict; isucc_lr1itemset=is0; contrib=c0} as t0)
      ({isucc_lr1itemset=is1; contrib=c1; _} as t1) =
    assert (equal_keys t0 t1);
    assert (Bool.( = ) (Lr1Itemset.is_empty is0) (Lr1Itemset.is_empty is1));
    let isucc_lr1itemset' = Lr1Itemset.diff is0 is1 in
    let contrib' = Contrib.diff c0 c1 in
    match Lr1Itemset.is_empty isucc_lr1itemset', Contrib.is_empty contrib' with
    | false, false -> {t0 with isucc_lr1itemset=isucc_lr1itemset'; contrib=contrib'}
    | false, true -> {t0 with isucc_lr1itemset=isucc_lr1itemset'}
    | true, false -> {t0 with contrib=Contrib.diff c0 c1}
    | true, true -> empty ~conflict_state_index ~symbol_index ~conflict
end
include T
include Identifiable.Make(T)

let resolutions ~resolve symbols prods {conflict=x0; contrib=c0; symbol_index; _}
  {conflict=x1; contrib=c1; symbol_index=symbol_index1; _} =
  assert (Contrib.equal x0 x1);
  assert Uns.(symbol_index = symbol_index1);
  (* Merge shift into contribs if present in the conflict manifestation, since all lanes are
   * implicated in shift actions. *)
  let c0, c1 = match Contrib.mem_shift x0 with
    | false -> c0, c1
    | true -> Contrib.(union shift c0), Contrib.(union shift c1)
  in
  (* Compute the resolutions (if enabled) of what the merged lane would receive from each input
   * lane. *)
  let r0, r1 = match resolve with
    | false -> c0, c1
    | true -> begin
        (Contrib.resolve symbols prods symbol_index c0),
        (Contrib.resolve symbols prods symbol_index c1)
      end
  in
  r0, r1

let equal_ielr1 ~resolve symbols prods t0 t1 =
  let r0, r1 = resolutions ~resolve symbols prods t0 t1 in
  Contrib.equal r0 r1

let compat_ielr1 ~resolve symbols prods t0 t1 =
  let r0, r1 = resolutions ~resolve symbols prods t0 t1 in
  (* Determine compatibility. *)
  match Contrib.length r0, Contrib.length r1 with
  | 0L, 0L -> begin
      (* By construction, at least one lane must be implicated in the conflict. *)
      not_reached ()
    end
  | 0L, _
  | _, 0L -> begin
      (* One of the lanes contributes nothing to the conflict, nor is there a shift action to be
       * implicated in. Unimplicated lanes are oblivious to merging. *)
      true
    end
  | 1L, 1L -> begin
      (* Resolution must be equal for lanes to be compatible. *)
      Contrib.equal r0 r1
    end
  | 1L, _
  | _, 1L -> begin
      (* One lane resolves, one doesn't. Different outcomes require splitting. *)
      false
    end
  | _, _ -> begin
      (* Both lanes result in conflict. The details of the conflicts don't matter, since merging
       * cannot cause resolution to succeed. *)
      true
    end
