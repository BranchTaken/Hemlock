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

    let pp_hr symbols prods {symbol_index; conflict; action} formatter =
      formatter
      |> Fmt.fmt "{symbol="
      |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
      |> Fmt.fmt "; conflict=" |> Contrib.pp_hr symbols prods conflict
      |> Fmt.fmt "; action=" |> State.Action.pp_hr symbols prods action
      |> Fmt.fmt "}"

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

module TraceVal = struct
  module T = struct
    (* Interstitial lane trace association between transition source/destination items. The mapping
     * implementation is 1:N, which is a canonical decomposition of the logical M:N mapping. The
     * source is a kernel item for interstitial states, a kernel or added item for contributing
     * states. *)
    type t = (Lr1Item.t, Lr1Itemset.t, Lr1Item.cmper_witness) Ordmap.t

    let hash_fold t state =
      state |> Ordmap.hash_fold Lr1Itemset.hash_fold t

    let cmp t0 t1 =
      Ordmap.cmp Lr1Itemset.cmp t0 t1

    let pp t formatter =
      formatter
      |> Ordmap.pp Lr1Itemset.pp t

    let fmt_hr symbols ?(alt=false) ?(width=0L) t formatter =
      formatter
      |> List.fmt ~alt ~width (fun (lr1item, lr1itemset) formatter ->
        formatter
        |> Fmt.fmt "(src="
        |> Lr1Item.pp_hr symbols lr1item
        |> Fmt.fmt ", dsts="
        |> Lr1Itemset.fmt_hr ~alt ~width:(width + 4L) symbols lr1itemset
      ) (Ordmap.to_alist t)
  end
  include T
  include Identifiable.Make(T)

  let length = Ordmap.length

  let init symbol_index ~lr1itemset ~isucc_lr1itemset =
    Lr1Itemset.fold ~init:(Ordmap.empty (module Lr1Item))
      ~f:(fun t Lr1Item.{lr0item; follow=follow_unfiltered} ->
        (* Filter the follow set to contain only `symbol_index`, since it is the only relevant
         * symbol in the context of kernel attribs. *)
        assert (Bitset.mem symbol_index follow_unfiltered);
        let follow = Bitset.singleton symbol_index in
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

  let fold = Ordmap.fold
end

type t = {
  (* Conflict state this lane context leads to. *)
  conflict_state: State.t;

  (* State this lane context immediately leads to. *)
  isucc: State.t;

  (* State corresponding to this lane context. *)
  state: State.t;

  (* Interstitial lane traces. Note that each trace key may correspond to multiple lanes, because
   * multiple kernel items in the conflict state can induce the same added ε production. *)
  traces: (TraceKey.t, TraceVal.t, TraceKey.cmper_witness) Ordmap.t;

  (* Memoized map of conflict attributions attributable to the lane(s) incompassing state->isucc
   * transit. *)
  kernel_attribs: KernelAttribs.t;
}

let pp {conflict_state; isucc; state; traces; kernel_attribs} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; isucc index=" |> Uns.pp (State.index isucc)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces count="
  |> Uns.pp (Ordmap.fold ~init:0L ~f:(fun accum (_, traceval) ->
    accum + (TraceVal.length traceval)) traces
  )
  |> Fmt.fmt "; kernel_attribs=" |> KernelAttribs.pp kernel_attribs
  |> Fmt.fmt "}"

let fmt_hr symbols prods ?(alt=false) ?(width=0L)
  {conflict_state; isucc; state; traces; kernel_attribs} formatter =
  formatter
  |> Fmt.fmt "{conflict_state index=" |> Uns.pp (State.index conflict_state)
  |> Fmt.fmt "; isucc index=" |> Uns.pp (State.index isucc)
  |> Fmt.fmt "; state index=" |> Uns.pp (State.index state)
  |> Fmt.fmt "; traces="
  |> List.fmt ~alt ~width:(width + 4L) (fun (tracekey, traceval) formatter ->
    formatter
    |> Fmt.fmt "{tracekey=" |> TraceKey.pp_hr symbols prods tracekey
    |> Fmt.fmt "; traceval=" |> TraceVal.fmt_hr symbols ~alt ~width:(width + 4L) traceval
    |> Fmt.fmt "}"
  ) (Ordmap.to_alist traces)
  |> Fmt.fmt "; kernel_attribs="
  |> KernelAttribs.fmt_hr symbols prods ~alt:true ~width:(width+4L) kernel_attribs
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

let cache_kernels_of_leftmost = true
let kernel_of_leftmost State.{statenub={lr1itemsetclosure; _}; _} Prod.{lhs_index=prod_lhs_index; _}
  symbol_index leftmost_cache =
  match cache_kernels_of_leftmost with
  | false ->
    Lr1ItemsetClosure.kernel_of_leftmost ~symbol_index ~lhs_index:prod_lhs_index lr1itemsetclosure,
    leftmost_cache
  | true ->
    Lr1ItemsetClosure.LeftmostCache.kernel_of_leftmost ~symbol_index ~lhs_index:prod_lhs_index
      lr1itemsetclosure leftmost_cache

let kernel_of_rightmost state prod symbol_index =
  (* Accumulate kernel items based on prod with the dot at the rightmost position and symbol_index
   * in the follow set. *)
  Lr1Itemset.fold ~init:Lr1Itemset.empty
    ~f:(fun accum lr1item ->
      let lr0item = lr1item.lr0item in
      match Prod.(lr0item.prod = prod)
            && Array.length lr0item.prod.rhs_indexes = lr0item.dot
            && Bitset.mem symbol_index lr1item.follow with
      | false -> accum
      | true -> Lr1Itemset.insert lr1item accum
    ) State.(state.statenub.lr1itemsetclosure.kernel)

let kernel_of_prod state symbol_index prod leftmost_cache =
  match Prod.(prod.rhs_indexes) with
  | [||] -> (* ε production, always associated with an added (non-kernel) item. *)
    kernel_of_leftmost state prod symbol_index leftmost_cache
  | _ -> kernel_of_rightmost state prod symbol_index, leftmost_cache

let kernel_of_prod_index prods state symbol_index prod_index leftmost_cache =
  let prod = Prods.prod_of_prod_index prod_index prods in
  kernel_of_prod state symbol_index prod leftmost_cache

let kernel_attribs {kernel_attribs; _} =
  kernel_attribs

let compute_kernel_attribs conflict_state traces =
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

let of_conflict_state ~resolve symbols prods leftmost_cache conflict_state =
  let traces, leftmost_cache = Attribs.fold
      ~init:(Ordmap.empty (module TraceKey), leftmost_cache)
      ~f:(fun (traces, leftmost_cache) {symbol_index; conflict; contrib; _} ->
        Ordset.fold ~init:(traces, leftmost_cache) ~f:(fun (traces, leftmost_cache) prod_index ->
          let action = State.Action.Reduce prod_index in
          let tracekey = TraceKey.init ~symbol_index ~conflict ~action in
          let lr1itemset, leftmost_cache =
            kernel_of_prod_index prods conflict_state symbol_index prod_index leftmost_cache in
          let traceval =
            TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset:Lr1Itemset.empty in
          let traces = Ordmap.amend tracekey ~f:(fun kernel_isuccs_opt ->
            match kernel_isuccs_opt with
            | None -> Some traceval
            | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
          ) traces in
          traces, leftmost_cache
        ) (Contrib.reduces contrib)
      ) (State.conflict_attribs ~resolve symbols prods conflict_state) in
  assert (not (Ordmap.is_empty traces));
  let kernel_attribs = compute_kernel_attribs conflict_state traces in
  let t = {
    conflict_state;
    isucc=conflict_state;
    state=conflict_state;
    traces;
    kernel_attribs;
  } in
  t, leftmost_cache

let of_ipred state leftmost_cache {conflict_state; state=isucc; traces=isucc_traces; _} =
  (* Create traces incrementally derived from those in `isucc_traces`. Some traces may terminate at
   * the isucc state; others may continue or even lead to forks. *)
  let traces, leftmost_cache = Ordmap.fold ~init:(Ordmap.empty (module TraceKey), leftmost_cache)
    ~f:(fun (traces, leftmost_cache)
      (TraceKey.{symbol_index; action; _} as tracekey, isucc_traceval) ->
      match action with
      | State.Action.ShiftPrefix _
      | ShiftAccept _ -> not_reached ()
      | Reduce _ -> begin
          TraceVal.fold ~init:(traces, leftmost_cache)
            ~f:(fun (traces, leftmost_cache)
              (Lr1Item.{lr0item=Lr0Item.{prod; dot=isucc_dot}; _}, _isucc_isucc_lr1itemset) ->
              match isucc_dot with
              | 0L -> (* The lane trace terminates at an attribution to the isucc's lr1item. *)
                traces, leftmost_cache
              | _ -> begin
                  let dot = pred isucc_dot in
                  let lr0item = Lr0Item.init ~prod ~dot in
                  (* Search for an item in state based on lr0item that has `symbol_index` in its
                   * follow set. *)
                  let lr1item_opt = Lr1Itemset.get
                      (Lr1Item.init ~lr0item
                          ~follow:(Bitset.singleton symbol_index))
                      (match dot with
                        | 0L -> State.(state.statenub.lr1itemsetclosure.added)
                        | _ -> State.(state.statenub.lr1itemsetclosure.kernel))
                  in
                  match lr1item_opt with
                  | None -> (* Lane doesn't encompass this state. *)
                    traces, leftmost_cache
                  | Some lr1item -> begin
                      let lr1itemset, leftmost_cache = match dot with
                        | 0L -> begin
                            (* Search for kernel items that have the item's LHS symbol just past
                             * their dots and `symbol_index` in their follow sets. *)
                            let kernel, leftmost_cache =
                              kernel_of_leftmost state prod symbol_index leftmost_cache in
                            let kernel = match Lr1Itemset.is_empty kernel with
                              | true ->
                                (* Contributing state. The trace source is an added item.
                                 * Attributable to all lanes leading to this state. *)
                                Lr1Itemset.singleton lr1item
                              | false ->
                                (* Interstitial state. The trace source is one or more kernel items.
                                *)
                                kernel
                            in
                            kernel, leftmost_cache
                          end
                        | _ -> (* Interstitial state. The trace source is a kernel item. *)
                          Lr1Itemset.singleton lr1item, leftmost_cache
                      in
                      let isucc_lr1itemset = TraceVal.lr1itemset isucc_traceval in
                      let traceval = TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset in
                      let traces = Ordmap.amend tracekey ~f:(fun traceval_opt ->
                        match traceval_opt with
                        | None -> Some traceval
                        | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
                      ) traces in
                      traces, leftmost_cache
                    end
                end
            ) isucc_traceval
        end
    ) isucc_traces
  in
  let kernel_attribs = compute_kernel_attribs conflict_state traces in
  let t = {
    conflict_state;
    isucc;
    state;
    traces;
    kernel_attribs
  } in
  t, leftmost_cache
