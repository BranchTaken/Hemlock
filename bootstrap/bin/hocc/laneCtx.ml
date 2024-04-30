open Basis
open! Basis.Rudiments

(* Key for an interstitial lane trace. *)
module TraceKey = struct
  module T = struct
    type t = {
      symbol_index: Symbol.Index.t; (* Conflicted symbol index. *)
      conflict: Contrib.t; (* Conflict manifestation. *)
      action: State.Action.t; (* Action *)
    }

    let hash_fold {symbol_index; conflict; action} state =
      state
      |> Symbol.Index.hash_fold symbol_index
      |> Contrib.hash_fold conflict
      |> State.Action.hash_fold action

    let cmp {symbol_index=s0; conflict=x0; action=action0}
      {symbol_index=s1; conflict=x1; action=action1} =
      let open Cmp in
      match Symbol.Index.cmp s0 s1 with
      | Lt -> Lt
      | Eq -> begin
          match Contrib.cmp x0 x1 with
          | Lt -> Lt
          | Eq -> State.Action.cmp action0 action1
          | Gt -> Gt
        end
      | Gt -> Gt

    let pp {symbol_index; conflict; action} formatter =
      formatter
      |> Fmt.fmt "{symbol_index=" |> Symbol.Index.pp symbol_index
      |> Fmt.fmt "; conflict=" |> Contrib.pp conflict
      |> Fmt.fmt "; action=" |> State.Action.pp action
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)

  let init ~symbol_index ~conflict ~action =
    {symbol_index; conflict; action}
end

(* Interstitial lane trace association between transition source/destination (aka ergo) items. The
 * mapping implementation is 1:N, which is a canonical decomposition of the logical M:N mapping. The
 * source is a kernel item for interstitial states, a kernel or added item for contributing states.
*)
module TraceVal = struct
  module T = struct
    type t = (Lr1Item.t, Lr1Itemset.t, Lr1Item.cmper_witness) Ordmap.t

    let hash_fold t state =
      state |> Ordmap.hash_fold Lr1Itemset.hash_fold t

    let cmp t0 t1 =
      Ordmap.cmp Lr1Itemset.cmp t0 t1

    let pp t formatter =
      formatter |> Ordmap.pp Lr1Itemset.pp t

    let fmt_hr symbols ?(alt=false) ?(width=0L) t formatter =
      List.fmt ~alt ~width (fun (lr1item, lr1itemset) formatter ->
        formatter
        |> Fmt.fmt "(src="
        |> Lr1Item.pp_hr symbols lr1item
        |> Fmt.fmt ", dsts="
        |> Lr1Itemset.fmt_hr ~alt ~width:(width + 4L) symbols lr1itemset
        |> Fmt.fmt ")"
      ) (Ordmap.to_alist t) formatter
  end
  include T
  include Identifiable.Make(T)

  let length = Ordmap.length

  let init symbol_index ~lr1itemset ~ergo_lr1itemset =
    Lr1Itemset.fold ~init:(Ordmap.empty (module Lr1Item))
      ~f:(fun t Lr1Item.{lr0item; follow=follow_unfiltered} ->
        (* Filter the follow set to contain only `symbol_index`, since it is the only relevant
         * symbol in the context of kernel contribs. *)
        assert (Ordset.mem symbol_index follow_unfiltered);
        let follow = Ordset.singleton (module Symbol.Index) symbol_index in
        let lr1item = Lr1Item.init ~lr0item ~follow in
        Ordmap.insert_hlt ~k:lr1item ~v:ergo_lr1itemset t
      ) lr1itemset

  let lr1itemset t =
    Ordmap.fold ~init:Lr1Itemset.empty ~f:(fun lr1itemset (lr1item, _ergo_lr1itemset) ->
      Lr1Itemset.insert_hlt lr1item lr1itemset
    ) t

  let union t0 t1 =
    Ordmap.union ~f:(fun _lr1item ergo_lr1itemset0 ergo_lr1itemset1 ->
      Lr1Itemset.union ergo_lr1itemset0 ergo_lr1itemset1
    ) t0 t1

  let fold_until = Ordmap.fold_until

  let fold = Ordmap.fold
end

type t = {
  (* Conflict state this lane context leads to. *)
  conflict_state: State.t;

  (* State this lane context immediately leads to. *)
  ergo: State.t;

  (* State corresponding to this lane context. *)
  state: State.t;

  (* Interstitial lane traces. Note that each trace key may have more than one corresponding lane,
   * because multiple kernel items in the conflict state can induce the same added ε production. *)
  traces: (TraceKey.t, TraceVal.t, TraceKey.cmper_witness) Ordmap.t;

  (* Conflict contributions directly to `state`->`ergo`. *)
  anon_contribs_direct: AnonContribs.t;
}

let pp {conflict_state; ergo; state; traces; anon_contribs_direct} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; ergo index=" |> Uns.pp (State.index ergo)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces count="
  |> Uns.pp (Ordmap.fold ~init:0L ~f:(fun accum (_, traceval) ->
    accum + (TraceVal.length traceval)) traces
  )
  |> Fmt.fmt "; anon_contribs_direct=" |> AnonContribs.pp anon_contribs_direct
  |> Fmt.fmt "}"

let fmt_hr symbols prods ?(alt=false) ?(width=0L)
  {conflict_state; ergo; state; traces; anon_contribs_direct} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; ergo index=" |> Uns.pp (State.index ergo)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces="
  |> List.fmt ~alt ~width:(width + 4L) (fun (tracekey, traceval) formatter ->
    formatter
    |> Fmt.fmt "{tracekey=" |> TraceKey.pp tracekey
    |> Fmt.fmt "; traceval=" |> TraceVal.fmt_hr symbols ~alt ~width:(width + 4L) traceval
    |> Fmt.fmt "}"
  ) (Ordmap.to_alist traces)
  |> Fmt.fmt "; anon_contribs_direct="
  |> AnonContribs.fmt_hr symbols prods ~alt ~width:(width + 4L) anon_contribs_direct
  |> Fmt.fmt "}"

let conflict_state {conflict_state; _} =
  conflict_state

let ergo {ergo; _} =
  ergo

let state {state; _} =
  state

let transit {state; ergo; _} =
  Transit.init ~src:(State.index state) ~dst:(State.index ergo)

let traces_length {traces; _} =
  Ordmap.length traces

let kernel_lr1itemset_of_leftmost state symbol_index prod =
  assert (not (Prod.is_synthetic prod));
  (* Accumulate kernel items with the LHS of prod just past the dot and symbol_index in the follow
   * set.
   *
   * Beware recursive productions, as in the following example involving nested ε productions. All
   * the reduces correspond to the same kernel item, but analysis of B needs to traverse A to reach
   * S. (S' is not reached in this example due to the follow set not containing ⊥.)
   *
   *   S' ::= · S ⊥ {ε}    kernel
   *   S ::= · A    {⊥}    added
   *   S ::= ·      {⊥}    added (reduce)
   *   A ::= · B    {⊥}    added
   *   A ::= ·      {⊥}    added (reduce)
   *   B ::= ·      {⊥}    added (reduce)
   *
   * Mark which symbols have been recursed on, in order to protect against infinite recursion on
   * e.g. `E ::= · E {t}`, as well as on mutually recursive items. *)
  let rec inner symbol_index prod State.{statenub={lr1itemsetclosure={kernel; added; _}; _}; _}
    marks accum = begin
    match Set.mem Prod.(prod.lhs_index) marks with
    | true -> marks, accum
    | false -> begin
        let marks = Set.insert Prod.(prod.lhs_index) marks in
        let accum = Lr1Itemset.fold ~init:accum ~f:(fun accum lr1item ->
          let lr0item = lr1item.lr0item in
          match Array.length lr0item.prod.rhs_indexes > lr0item.dot
                && Array.get lr0item.dot lr0item.prod.rhs_indexes = Prod.(prod.lhs_index)
                && Ordset.mem symbol_index lr1item.follow with
          | false -> accum
          | true -> Lr1Itemset.insert lr1item accum
        ) kernel in
        (* Search the added set for items with the LHS of prod just past the dot and symbol_index in
         * the follow set, and recurse on the items. *)
        let marks, accum = Lr1Itemset.fold ~init:(marks, accum) ~f:(fun (marks, accum) lr1item ->
          let lr0item = lr1item.lr0item in
          match Array.length lr0item.prod.rhs_indexes > lr0item.dot
                && Array.get lr0item.dot lr0item.prod.rhs_indexes = Prod.(prod.lhs_index)
                && Ordset.mem symbol_index lr1item.follow with
          | false -> marks, accum
          | true -> inner symbol_index lr0item.prod state marks accum
        ) added in
        marks, accum
      end
  end in
  let _marks, accum =
    inner symbol_index prod state (Set.empty (module Symbol.Index)) Lr1Itemset.empty in
  accum

let kernel_lr1itemset_of_rightmost state symbol_index prod =
  (* Accumulate kernel items based on prod with the dot at the rightmost position and symbol_index
   * in the follow set. *)
  Lr1Itemset.fold ~init:Lr1Itemset.empty
    ~f:(fun accum lr1item ->
      let lr0item = lr1item.lr0item in
      match Prod.(lr0item.prod = prod)
            && Array.length lr0item.prod.rhs_indexes = lr0item.dot
            && Ordset.mem symbol_index lr1item.follow with
      | false -> accum
      | true -> Lr1Itemset.insert lr1item accum
    ) State.(state.statenub.lr1itemsetclosure.kernel)

let kernel_lr1itemset_of_prod state symbol_index prod =
  match Prod.(prod.rhs_indexes) with
  | [||] -> (* ε production, always associated with an added (non-kernel) item. *)
    kernel_lr1itemset_of_leftmost state symbol_index prod
  | _ -> kernel_lr1itemset_of_rightmost state symbol_index prod

let kernel_lr1itemset_of_prod_index prods state symbol_index prod_index =
  let prod = Prods.prod_of_prod_index prod_index prods in
  kernel_lr1itemset_of_prod state symbol_index prod

let kernel_contribs {conflict_state; traces; _} =
  let conflict_state_index = State.index conflict_state in
  Ordmap.fold ~init:KernelContribs.empty
    ~f:(fun kernel_contribs (TraceKey.{symbol_index; conflict; action}, kernel_ergos) ->
      let contrib = match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> Contrib.init_reduce prod_index
      in
      TraceVal.fold ~init:kernel_contribs ~f:(fun kernel_contribs (lr1item, ergo_lr1itemset) ->
        let attrib = Attrib.init ~symbol_index ~conflict ~ergo_lr1itemset ~contrib in
        let trace_contribs = Contribs.singleton ~conflict_state_index attrib in
        KernelContribs.insert lr1item trace_contribs kernel_contribs
      ) kernel_ergos
    ) traces

let anon_contribs ({anon_contribs_direct; _} as t) =
  KernelContribs.fold ~init:AnonContribs.empty ~f:(fun anon_contribs (_lr1item, contribs) ->
    Contribs.fold ~init:AnonContribs.empty
      ~f:(fun anon_contribs conflict_state_index Attrib.{symbol_index; contrib; _} ->
        AnonContribs.insert ~conflict_state_index symbol_index contrib anon_contribs
      ) contribs
    |> AnonContribs.union anon_contribs
  ) (kernel_contribs t)
  |> AnonContribs.union anon_contribs_direct

let anon_contribs_direct {anon_contribs_direct; _} =
  anon_contribs_direct

let of_conflict_state ~resolve symbols prods conflict_state =
  let conflict_state_index = State.index conflict_state in
  let traces, anon_contribs_direct = Attribs.fold
      ~init:(Ordmap.empty (module TraceKey), AnonContribs.empty)
      ~f:(fun (traces, anon_contribs_direct) Attrib.{symbol_index; conflict; contrib; _} ->
        let anon_contribs_direct = match Contrib.mem_shift contrib with
          | false -> anon_contribs_direct
          | true ->
            AnonContribs.insert ~conflict_state_index symbol_index Contrib.shift
              anon_contribs_direct
        in
        let traces = Ordset.fold ~init:traces ~f:(fun traces prod_index ->
          let action = State.Action.Reduce prod_index in
          let tracekey = TraceKey.init ~symbol_index ~conflict ~action in
          let lr1itemset =
            kernel_lr1itemset_of_prod_index prods conflict_state symbol_index prod_index in
          let traceval = TraceVal.init symbol_index ~lr1itemset ~ergo_lr1itemset:Lr1Itemset.empty in
          Ordmap.amend tracekey ~f:(fun kernel_ergos_opt ->
            match kernel_ergos_opt with
            | None -> Some traceval
            | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
          ) traces
        ) (Contrib.reduces contrib) in
        traces, anon_contribs_direct
      ) (State.conflict_attribs ~resolve symbols prods conflict_state) in
  assert (not (Ordmap.is_empty traces));
  {
    conflict_state;
    ergo=conflict_state;
    state=conflict_state;
    traces;
    anon_contribs_direct;
  }

let of_ante state {conflict_state; state=ergo; traces=ergo_traces; _} =
  let conflict_state_index = State.index conflict_state in
  (* Create traces incrementally derived from those in `ergo_traces`. Some traces may terminate at
   * the ergo state; others may continue or even lead to forks. *)
  let traces, anon_contribs_direct = Ordmap.fold
      ~init:(Ordmap.empty (module TraceKey), AnonContribs.empty)
      ~f:(fun (traces, anon_contribs_direct) (TraceKey.{symbol_index; action; _} as tracekey,
        ergo_traceval) ->
        match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> begin
            TraceVal.fold ~init:(traces, anon_contribs_direct)
              ~f:(fun (traces, anon_contribs_direct) (ergo_lr1item, _ergo_ergo_lr1itemset) ->
                let ergo_lr0item = Lr1Item.(ergo_lr1item.lr0item) in
                match ergo_lr0item.dot with
                | 0L -> begin
                    (* The lane trace terminates at a direct contribution by `ergo_lr1item`. *)
                    traces, anon_contribs_direct
                  end
                | _ -> begin
                    let prod = ergo_lr0item.prod in
                    let dot = pred ergo_lr0item.dot in
                    let lr0item = Lr0Item.init ~prod ~dot in
                    (* Search for an item in state based on lr0item that has `symbol_index` in its
                     * follow set. *)
                    let lr1item_opt = Lr1Itemset.fold_until ~init:None ~f:(fun _ lr1item ->
                      match Lr0Item.(lr1item.lr0item = lr0item)
                            && Ordset.mem symbol_index lr1item.follow with
                      | false -> None, false
                      | true -> Some lr1item, true
                    ) (match dot with
                      | 0L -> State.(state.statenub.lr1itemsetclosure.added)
                      | _ -> State.(state.statenub.lr1itemsetclosure.kernel)
                    )
                    in
                    match lr1item_opt with
                    | None -> (* Lane doesn't encompass this state. *)
                      traces, anon_contribs_direct
                    | Some lr1item -> begin
                        match dot with
                        | 0L -> begin
                            (* Search for kernel items that have the item's LHS symbol just past
                             * their dots and `symbol_index` in their follow sets. *)
                            let lr1itemset = kernel_lr1itemset_of_leftmost state symbol_index
                                lr1item.lr0item.prod in
                            match Lr1Itemset.is_empty lr1itemset with
                            | true -> begin
                                (* Contributing state. The trace source is an added item. *)
                                let lr1itemset = Lr1Itemset.singleton lr1item in
                                let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                                  let ergo_lr1itemset = TraceVal.lr1itemset ergo_traceval in
                                  let traceval =
                                    TraceVal.init symbol_index ~lr1itemset ~ergo_lr1itemset in
                                  match traceval_opt with
                                  | None -> Some traceval
                                  | Some traceval_existing ->
                                    Some (TraceVal.union traceval traceval_existing)
                                ) traces in
                                (* Attributable to all lanes leading to this state. *)
                                let anon_contribs_direct =
                                  AnonContribs.amend ~conflict_state_index symbol_index
                                    ~f:(fun contrib_opt ->
                                      let contrib = Contrib.init_reduce prod_index in
                                      match contrib_opt with
                                      | None -> Some contrib
                                      | Some contrib_existing ->
                                        Some (Contrib.union contrib contrib_existing)
                                    ) anon_contribs_direct in
                                traces, anon_contribs_direct
                              end
                            | false -> begin
                                (* Interstitial state. The trace source is one or more kernel items.
                                *)
                                let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                                  let ergo_lr1itemset = TraceVal.lr1itemset ergo_traceval in
                                  let traceval =
                                    TraceVal.init symbol_index ~lr1itemset ~ergo_lr1itemset in
                                  match traceval_opt with
                                  | None -> Some traceval
                                  | Some traceval_existing ->
                                    Some (TraceVal.union traceval traceval_existing)
                                ) traces in
                                traces, anon_contribs_direct
                              end
                          end
                        | _ -> begin
                            (* Interstitial state. The trace source is a kernel item. *)
                            let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                              let lr1itemset = Lr1Itemset.singleton lr1item in
                              let ergo_lr1itemset = TraceVal.lr1itemset ergo_traceval in
                              let traceval =
                                TraceVal.init symbol_index ~lr1itemset ~ergo_lr1itemset in
                              match traceval_opt with
                              | None -> Some traceval
                              | Some traceval_existing ->
                                Some (TraceVal.union traceval traceval_existing)
                            ) traces in
                            traces, anon_contribs_direct
                          end
                      end
                  end
              ) ergo_traceval
          end
      ) ergo_traces
  in
  {
    conflict_state;
    ergo;
    state;
    traces;
    anon_contribs_direct;
  }

let post_init ante_lanectxs ({conflict_state; traces; anon_contribs_direct; _} as t) =
  let conflict_state_index = State.index conflict_state in
  (* A lane trace in this lane context makes a direct contribution if the lane does not extend back
   * to any antecedents. This situation is detected in `of_ante` when the trace source is an added
   * item, so it's sufficient to look only at traces with kernel items as sources. *)
  let anon_contribs_direct = Ordmap.fold ~init:anon_contribs_direct
      ~f:(fun anon_contribs_direct (TraceKey.{symbol_index; action; _}, traceval) ->
        match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> begin
            TraceVal.fold ~init:anon_contribs_direct
              ~f:(fun anon_contribs_direct (src, _ergo_dsts) ->
                match Lr1Item.(src.lr0item.dot) with
                | 0L -> anon_contribs_direct (* Source is an added item. *)
                | _ -> begin
                    let lane_extends = List.fold_until ~init:false ~f:(fun _ ante_lanectx ->
                      let lane_extends = Ordmap.fold_until ~init:false
                          ~f:(fun _ (_ante_tracekey, ante_traceval) ->
                            let lane_extends = TraceVal.fold_until ~init:false
                                ~f:(fun _ (_ante_src, dsts) ->
                                  let lane_extends = Lr1Itemset.mem src dsts in
                                  lane_extends, lane_extends
                                ) ante_traceval in
                            lane_extends, lane_extends
                          ) ante_lanectx.traces
                      in
                      lane_extends, lane_extends
                    ) ante_lanectxs in
                    match lane_extends with
                    | true -> anon_contribs_direct
                    | false -> begin
                        AnonContribs.amend ~conflict_state_index symbol_index
                          ~f:(fun contrib_opt ->
                            let contrib = Contrib.init_reduce prod_index in
                            match contrib_opt with
                            | None -> Some contrib
                            | Some contrib_existing ->
                              Some (Contrib.union contrib contrib_existing)
                          ) anon_contribs_direct
                      end
                  end
              ) traceval
          end
      ) traces in
  {t with anon_contribs_direct}
