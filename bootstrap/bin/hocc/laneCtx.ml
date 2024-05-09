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

(* Interstitial lane trace association between transition source/destination items. The mapping
 * implementation is 1:N, which is a canonical decomposition of the logical M:N mapping. The source
 * is a kernel item for interstitial states, a kernel or added item for contributing states. *)
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

  let init symbol_index ~lr1itemset ~isucc_lr1itemset =
    Lr1Itemset.fold ~init:(Ordmap.empty (module Lr1Item))
      ~f:(fun t Lr1Item.{lr0item; follow=follow_unfiltered} ->
        (* Filter the follow set to contain only `symbol_index`, since it is the only relevant
         * symbol in the context of kernel attribs. *)
        assert (Ordset.mem symbol_index follow_unfiltered);
        let follow = Ordset.singleton (module Symbol.Index) symbol_index in
        let lr1item = Lr1Item.init ~lr0item ~follow in
        Ordmap.insert_hlt ~k:lr1item ~v:isucc_lr1itemset t
      ) lr1itemset

  let lr1itemset t =
    Ordmap.fold ~init:Lr1Itemset.empty ~f:(fun lr1itemset (lr1item, _isucc_lr1itemset) ->
      Lr1Itemset.insert_hlt lr1item lr1itemset
    ) t

  let union t0 t1 =
    Ordmap.union ~f:(fun _lr1item isucc_lr1itemset0 isucc_lr1itemset1 ->
      Lr1Itemset.union isucc_lr1itemset0 isucc_lr1itemset1
    ) t0 t1

  let fold_until = Ordmap.fold_until

  let fold = Ordmap.fold
end

type t = {
  (* Conflict state this lane context leads to. *)
  conflict_state: State.t;

  (* State this lane context immediately leads to. *)
  isucc: State.t;

  (* State corresponding to this lane context. *)
  state: State.t;

  (* Interstitial lane traces. Note that each trace key may have more than one corresponding lane,
   * because multiple kernel items in the conflict state can induce the same added ε production. *)
  traces: (TraceKey.t, TraceVal.t, TraceKey.cmper_witness) Ordmap.t;

  (* Lane conflict attributions directly to `state`->`isucc`. *)
  lane_attribs_direct: Attribs.t;
}

let pp {conflict_state; isucc; state; traces; lane_attribs_direct} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; isucc index=" |> Uns.pp (State.index isucc)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces count="
  |> Uns.pp (Ordmap.fold ~init:0L ~f:(fun accum (_, traceval) ->
    accum + (TraceVal.length traceval)) traces
  )
  |> Fmt.fmt "; lane_attribs_direct=" |> Attribs.pp lane_attribs_direct
  |> Fmt.fmt "}"

let fmt_hr symbols prods ?(alt=false) ?(width=0L)
  {conflict_state; isucc; state; traces; lane_attribs_direct} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; isucc index=" |> Uns.pp (State.index isucc)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces="
  |> List.fmt ~alt ~width:(width + 4L) (fun (tracekey, traceval) formatter ->
    formatter
    |> Fmt.fmt "{tracekey=" |> TraceKey.pp tracekey
    |> Fmt.fmt "; traceval=" |> TraceVal.fmt_hr symbols ~alt ~width:(width + 4L) traceval
    |> Fmt.fmt "}"
  ) (Ordmap.to_alist traces)
  |> Fmt.fmt "; lane_attribs_direct="
  |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) lane_attribs_direct
  |> Fmt.fmt "}"

let conflict_state {conflict_state; _} =
  conflict_state

let isucc {isucc; _} =
  isucc

let state {state; _} =
  state

let transit {state; isucc; _} =
  Transit.init ~src:(State.index state) ~dst:(State.index isucc)

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

let kernel_attribs {conflict_state; traces; _} =
  let conflict_state_index = State.index conflict_state in
  Ordmap.fold ~init:KernelAttribs.empty
    ~f:(fun kernel_attribs (TraceKey.{symbol_index; conflict; action}, kernel_isuccs) ->
      let contrib = match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> Contrib.init_reduce prod_index
      in
      TraceVal.fold ~init:kernel_attribs ~f:(fun kernel_attribs (lr1item, isucc_lr1itemset) ->
        let attrib =
          Attrib.init ~conflict_state_index ~symbol_index ~conflict ~isucc_lr1itemset ~contrib in
        let trace_attribs = Attribs.singleton attrib in
        KernelAttribs.insert lr1item trace_attribs kernel_attribs
      ) kernel_isuccs
    ) traces

let lane_attribs ({lane_attribs_direct; _} as t) =
  KernelAttribs.fold ~init:Attribs.empty ~f:(fun lane_attribs (_lr1item, attribs) ->
    Attribs.fold ~init:Attribs.empty
      ~f:(fun lane_attribs {conflict_state_index; symbol_index; conflict; contrib; _} ->
        let attrib = Attrib.init_lane ~conflict_state_index ~symbol_index ~conflict ~contrib in
        Attribs.insert attrib lane_attribs
      ) attribs
    |> Attribs.union lane_attribs
  ) (kernel_attribs t)
  |> Attribs.union lane_attribs_direct

let lane_attribs_direct {lane_attribs_direct; _} =
  lane_attribs_direct

let of_conflict_state ~resolve symbols prods conflict_state =
  let conflict_state_index = State.index conflict_state in
  let traces, lane_attribs_direct = Attribs.fold
      ~init:(Ordmap.empty (module TraceKey), Attribs.empty)
      ~f:(fun (traces, lane_attribs_direct) {symbol_index; conflict; contrib; _} ->
        let lane_attribs_direct = match Contrib.mem_shift contrib with
          | false -> lane_attribs_direct
          | true -> begin
              let attrib = Attrib.init_lane ~conflict_state_index ~symbol_index ~conflict
                  ~contrib:Contrib.shift in
              Attribs.insert attrib lane_attribs_direct
            end
        in
        let traces = Ordset.fold ~init:traces ~f:(fun traces prod_index ->
          let action = State.Action.Reduce prod_index in
          let tracekey = TraceKey.init ~symbol_index ~conflict ~action in
          let lr1itemset =
            kernel_lr1itemset_of_prod_index prods conflict_state symbol_index prod_index in
          let traceval =
            TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset:Lr1Itemset.empty in
          Ordmap.amend tracekey ~f:(fun kernel_isuccs_opt ->
            match kernel_isuccs_opt with
            | None -> Some traceval
            | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
          ) traces
        ) (Contrib.reduces contrib) in
        traces, lane_attribs_direct
      ) (State.conflict_attribs ~resolve symbols prods conflict_state) in
  assert (not (Ordmap.is_empty traces));
  {
    conflict_state;
    isucc=conflict_state;
    state=conflict_state;
    traces;
    lane_attribs_direct;
  }

let of_ipred state {conflict_state; state=isucc; traces=isucc_traces; _} =
  let conflict_state_index = State.index conflict_state in
  (* Create traces incrementally derived from those in `isucc_traces`. Some traces may terminate at
   * the isucc state; others may continue or even lead to forks. *)
  let traces, lane_attribs_direct = Ordmap.fold
      ~init:(Ordmap.empty (module TraceKey), Attribs.empty)
      ~f:(fun (traces, lane_attribs_direct) (TraceKey.{symbol_index; conflict; action} as tracekey,
        isucc_traceval) ->
        match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> begin
            TraceVal.fold ~init:(traces, lane_attribs_direct)
              ~f:(fun (traces, lane_attribs_direct) (isucc_lr1item, _isucc_isucc_lr1itemset) ->
                let isucc_lr0item = Lr1Item.(isucc_lr1item.lr0item) in
                match isucc_lr0item.dot with
                | 0L -> begin
                    (* The lane trace terminates at a direct attribution by `isucc_lr1item`. *)
                    traces, lane_attribs_direct
                  end
                | _ -> begin
                    let prod = isucc_lr0item.prod in
                    let dot = pred isucc_lr0item.dot in
                    let lr0item = Lr0Item.init ~prod ~dot in
                    (* Search for an item in state based on lr0item that has `symbol_index` in its
                     * follow set. *)
                    let lr1item_opt = Lr1Itemset.get
                        (Lr1Item.init ~lr0item
                            ~follow:(Ordset.singleton (module Symbol.Index) symbol_index))
                        (match dot with
                          | 0L -> State.(state.statenub.lr1itemsetclosure.added)
                          | _ -> State.(state.statenub.lr1itemsetclosure.kernel))
                    in
                    match lr1item_opt with
                    | None -> (* Lane doesn't encompass this state. *)
                      traces, lane_attribs_direct
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
                                  let isucc_lr1itemset = TraceVal.lr1itemset isucc_traceval in
                                  let traceval =
                                    TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset in
                                  match traceval_opt with
                                  | None -> Some traceval
                                  | Some traceval_existing ->
                                    Some (TraceVal.union traceval traceval_existing)
                                ) traces in
                                (* Attributable to all lanes leading to this state. *)
                                let lane_attribs_direct =
                                  Attribs.amend ~conflict_state_index symbol_index
                                    ~f:(fun attrib_opt ->
                                      let contrib = Contrib.init_reduce prod_index in
                                      let attrib = Attrib.init_lane ~conflict_state_index
                                          ~symbol_index ~conflict ~contrib in
                                      match attrib_opt with
                                      | None -> Some attrib
                                      | Some attrib_existing ->
                                        Some (Attrib.union attrib attrib_existing)
                                    ) lane_attribs_direct in
                                traces, lane_attribs_direct
                              end
                            | false -> begin
                                (* Interstitial state. The trace source is one or more kernel items.
                                *)
                                let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                                  let isucc_lr1itemset = TraceVal.lr1itemset isucc_traceval in
                                  let traceval =
                                    TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset in
                                  match traceval_opt with
                                  | None -> Some traceval
                                  | Some traceval_existing ->
                                    Some (TraceVal.union traceval traceval_existing)
                                ) traces in
                                traces, lane_attribs_direct
                              end
                          end
                        | _ -> begin
                            (* Interstitial state. The trace source is a kernel item. *)
                            let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                              let lr1itemset = Lr1Itemset.singleton lr1item in
                              let isucc_lr1itemset = TraceVal.lr1itemset isucc_traceval in
                              let traceval =
                                TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset in
                              match traceval_opt with
                              | None -> Some traceval
                              | Some traceval_existing ->
                                Some (TraceVal.union traceval traceval_existing)
                            ) traces in
                            traces, lane_attribs_direct
                          end
                      end
                  end
              ) isucc_traceval
          end
      ) isucc_traces
  in
  {
    conflict_state;
    isucc;
    state;
    traces;
    lane_attribs_direct;
  }

let post_init ipred_lanectxs ({conflict_state; traces; lane_attribs_direct; _} as t) =
  let conflict_state_index = State.index conflict_state in
  (* A lane trace in this lane context has a direct attribution if the lane does not extend back to
   * any predecessors. This situation is handled in `of_ipred` when the trace source is an added
   * item, so it suffices here to process only traces with kernel items as sources. *)
  let lane_attribs_direct = Ordmap.fold ~init:lane_attribs_direct
      ~f:(fun lane_attribs_direct (TraceKey.{symbol_index; conflict; action}, traceval) ->
        match action with
        | State.Action.ShiftPrefix _
        | ShiftAccept _ -> not_reached ()
        | Reduce prod_index -> begin
            TraceVal.fold ~init:lane_attribs_direct
              ~f:(fun lane_attribs_direct (src, _isucc_dsts) ->
                match Lr1Item.(src.lr0item.dot) with
                | 0L -> lane_attribs_direct (* Source is an added item. *)
                | _ -> begin
                    let lane_extends = List.fold_until ~init:false ~f:(fun _ ipred_lanectx ->
                      let lane_extends = Ordmap.fold_until ~init:false
                          ~f:(fun _ (_ipred_tracekey, ipred_traceval) ->
                            let lane_extends = TraceVal.fold_until ~init:false
                                ~f:(fun _ (_ipred_src, dsts) ->
                                  let lane_extends = Lr1Itemset.mem src dsts in
                                  lane_extends, lane_extends
                                ) ipred_traceval in
                            lane_extends, lane_extends
                          ) ipred_lanectx.traces
                      in
                      lane_extends, lane_extends
                    ) ipred_lanectxs in
                    match lane_extends with
                    | true -> lane_attribs_direct
                    | false -> begin
                        Attribs.amend ~conflict_state_index symbol_index ~f:(fun attrib_opt ->
                          let contrib = Contrib.init_reduce prod_index in
                          let attrib = Attrib.init_lane ~conflict_state_index ~symbol_index
                              ~conflict ~contrib in
                          match attrib_opt with
                          | None -> Some attrib
                          | Some contrib_existing -> Some (Attrib.union attrib contrib_existing)
                        ) lane_attribs_direct
                      end
                  end
              ) traceval
          end
      ) traces in
  {t with lane_attribs_direct}
