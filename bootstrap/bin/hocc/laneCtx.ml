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

(*
let kernel_of_leftmost state symbol_index prod_lhs_index =
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
  let rec inner kernel added symbol_index prod_lhs_index marks accum = begin
    let marks = Ordset.insert prod_lhs_index marks in
    let accum = Lr1Itemset.fold ~init:accum
        ~f:(fun accum
          (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
          match Array.length rhs_indexes > dot
                && Symbol.Index.( = ) (Array.get dot rhs_indexes) prod_lhs_index
                && Ordset.mem symbol_index follow with
          | false -> accum
          | true -> Lr1Itemset.insert lr1item accum
        ) kernel in
    (* Search the added set for items with the LHS of prod just past the dot and symbol_index in the
     * follow set, and recurse on the items. *)
    let marks, accum = Lr1Itemset.fold ~init:(marks, accum)
      ~f:(fun (marks, accum)
        (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; rhs_indexes; _}; _}; follow}) ->
        match Ordset.mem lhs_index marks with
        | true -> marks, accum
        | false -> begin
            (* The dot is always at position 0 in added items. *)
            match Array.length rhs_indexes > 0L
                  && Symbol.Index.( = ) (Array.get 0L rhs_indexes) prod_lhs_index
                  && Ordset.mem symbol_index follow with
            | false -> marks, accum
            | true -> inner kernel added symbol_index lhs_index marks accum
          end
      ) added in
    marks, accum
  end in
  let State.{statenub={lr1itemsetclosure={kernel; added; _}; _}; _} = state in
  let _marks, accum = inner kernel added symbol_index prod_lhs_index
      (Ordset.empty (module Symbol.Index)) Lr1Itemset.empty in
  accum
*)

module K = struct
  module T = struct
    type t = {
      prod_lhs_index: Symbol.Index.t;
      symbol_index: Symbol.Index.t;
    }

    let hash_fold {prod_lhs_index; symbol_index} state =
      state
      |> Symbol.Index.hash_fold prod_lhs_index
      |> Symbol.Index.hash_fold symbol_index

    let cmp {prod_lhs_index=pli0; symbol_index=si0} {prod_lhs_index=pli1; symbol_index=si1} =
      let open Cmp in
      match Symbol.Index.cmp pli0 pli1 with
      | Lt -> Lt
      | Eq -> Symbol.Index.cmp si0 si1
      | Gt -> Gt

    let pp {prod_lhs_index; symbol_index} formatter =
      formatter
      |> Fmt.fmt "{prod_lhs_index=" |> Symbol.Index.pp prod_lhs_index
      |> Fmt.fmt "; symbol_index=" |> Symbol.Index.pp symbol_index
      |> Fmt.fmt "}"

    let init ~prod_lhs_index ~symbol_index =
      {prod_lhs_index; symbol_index}
  end
  include T
  include Identifiable.Make(T)
end

let kernels_of_leftmost state prod_lhs_index symbol_indexes =
  (* Accumulate kernel items with the prod LHS just past the dot and symbol_index in the follow set.
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
  let rec inner kernel added prod_lhs_index inner_lhs_index symbol_indexes marks accum = begin
    let marks = Ordmap.amend inner_lhs_index ~f:(fun lhs_index_opt ->
      match lhs_index_opt with
      | None -> Some symbol_indexes
      | Some symbol_indexes_prev -> Some (Ordset.union symbol_indexes symbol_indexes_prev)
    ) marks in
    let accum = Lr1Itemset.fold ~init:accum
        ~f:(fun accum
          (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
          match Array.length rhs_indexes > dot
                && Symbol.Index.( = ) (Array.get dot rhs_indexes) inner_lhs_index with
          | false -> accum
          | true -> begin
              Ordset.fold ~init:accum ~f:(fun accum symbol_index ->
                match Ordset.mem symbol_index follow with
                | false -> accum
                | true -> begin
                    let k = K.init ~prod_lhs_index ~symbol_index in
                    Ordmap.amend k ~f:(fun kernel_opt ->
                      match kernel_opt with
                      | None -> Some (Lr1Itemset.singleton lr1item)
                      | Some kernel -> Some (Lr1Itemset.insert lr1item kernel)
                    ) accum
                  end
              ) symbol_indexes
            end
        ) kernel in
    (* Search the added set for items with the prod LHS just past the dot and symbol_index in the
     * follow set. *)
    let found = Lr1Itemset.fold ~init:(Ordmap.empty (module Symbol.Index))
      ~f:(fun found
        (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; rhs_indexes; _}; _}; follow}) ->
        let marked, symbol_indexes = match Ordmap.get lhs_index marks with
          | None -> false, symbol_indexes
          | Some marked_symbol_indexes -> begin
              let symbol_indexes = Ordset.diff symbol_indexes marked_symbol_indexes in
              Ordset.is_empty symbol_indexes, symbol_indexes
            end
        in
        match marked with
        | true -> found
        | false -> begin
            (* The dot is always at position 0 in added items. *)
            match Array.length rhs_indexes > 0L
                  && Symbol.Index.( = ) (Array.get 0L rhs_indexes) inner_lhs_index with
            | false -> found
            | true -> begin
                let symbol_indexes' = Ordset.inter symbol_indexes follow in
                match Ordset.is_empty symbol_indexes' with
                | true -> found
                | false -> begin
                    Ordmap.amend lhs_index ~f:(fun symbol_indexes_opt ->
                      match symbol_indexes_opt with
                      | None -> Some symbol_indexes'
                      | Some symbol_indexes -> Some (Ordset.union symbol_indexes' symbol_indexes)
                    ) found
                  end
              end
          end
    ) added in
    (* Recurse on the symbols corresponding to items found in the added set search. *)
    let marks, accum = Ordmap.fold ~init:(marks, accum)
      ~f:(fun (marks, accum) (lhs_index, symbol_indexes) ->
        inner kernel added prod_lhs_index lhs_index symbol_indexes marks accum
      ) found in
    marks, accum
  end in
  let State.{statenub={lr1itemsetclosure={kernel; added; _}; _}; _} = state in
  let marks = Ordmap.empty (module Symbol.Index) in
  let accum = Ordmap.empty (module K) in
  let _marks, accum = inner kernel added prod_lhs_index prod_lhs_index symbol_indexes marks accum in
(*
  File.Fmt.stderr
  |> Fmt.fmt "XXX kernels_of_leftmost state_index="
  |> State.Index.pp (State.index state)
  |> Fmt.fmt "; prod_lhs_index=" |> Symbol.Index.pp prod_lhs_index
  |> Fmt.fmt "; symbol_indexes=" |> Ordset.pp symbol_indexes
  |> Fmt.fmt " -> " |> Ordmap.pp (fun _lr1itemset formatter -> formatter) accum
  |> Fmt.fmt "\n"
  |> ignore;
*)
  accum

let state_lhs_symbol_indexes State.{statenub={lr1itemsetclosure={kernel; added; _}; _}; _} =
  let lhs_symbol_indexes = Ordmap.empty (module Symbol.Index) in
  let lhs_symbol_indexes = Lr1Itemset.fold ~init:lhs_symbol_indexes
      ~f:(fun lhs_symbol_indexes
        Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _} as prod; _}; follow} ->
        match Prod.is_synthetic prod with
        | true -> lhs_symbol_indexes
        | false -> Ordmap.insert ~k:lhs_index ~v:follow lhs_symbol_indexes
      ) kernel
  in
  let lhs_symbol_indexes = Lr1Itemset.fold ~init:lhs_symbol_indexes
      ~f:(fun lhs_symbol_indexes Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _}; _}; follow} ->
        Ordmap.insert ~k:lhs_index ~v:follow lhs_symbol_indexes
      ) added
  in
  lhs_symbol_indexes

let state_lhs_symbol_indexes_cache = ref (Ordmap.empty (module State.Index))
let state_lhs_symbol_indexes
    (State.{statenub={lr1itemsetclosure={index=state_index; _}; _}; _} as state) =
  match Ordmap.get state_index !state_lhs_symbol_indexes_cache with
  | Some state_lhs_symbol_indexes -> state_lhs_symbol_indexes
  | None -> begin
      let state_lhs_symbol_indexes = state_lhs_symbol_indexes state in
      state_lhs_symbol_indexes_cache :=
        Ordmap.insert ~k:state_index ~v:state_lhs_symbol_indexes !state_lhs_symbol_indexes_cache;
      state_lhs_symbol_indexes
    end

let cached = ref (Ordmap.empty (module State.Index))
let cache = ref (Ordmap.empty (module State.Index))
let kernel_of_leftmost
    (State.{statenub={lr1itemsetclosure={index=state_index; _}; _}; _} as state)
    Prod.{lhs_index=prod_lhs_index; _}
    symbol_index =
  let kkmap = match Ordmap.get state_index !cache with
    | None -> begin
        let kkmap = Ordmap.empty (module K) in
        let state_lhs_symbol_indexes = state_lhs_symbol_indexes state in
        let kkmap = match Ordmap.get prod_lhs_index state_lhs_symbol_indexes with
        | None -> kkmap
        | Some symbol_indexes -> begin
            kernels_of_leftmost state prod_lhs_index symbol_indexes
            |> Ordmap.union ~f:(fun _k kernel0 kernel1 -> Lr1Itemset.union kernel0 kernel1) kkmap
          end
        in
        cached := Ordmap.insert_hlt ~k:state_index
          ~v:(Ordset.singleton (module Symbol.Index) prod_lhs_index) !cached;
        cache := Ordmap.insert ~k:state_index ~v:kkmap !cache;
(*
        File.Fmt.stderr
        |> Fmt.fmt "XXX A state_index=" |> State.Index.pp state_index
        |> Fmt.fmt "; kkmap="
        |> Ordmap.pp (fun _lr1itemset formatter -> formatter) kkmap
        |> Fmt.fmt "\n"
        |> ignore;
*)
        kkmap
      end
    | Some kkmap -> begin
        match Ordset.mem prod_lhs_index (Ordmap.get_hlt state_index !cached) with
        | true -> kkmap
        | false -> begin
            let state_lhs_symbol_indexes = state_lhs_symbol_indexes state in
            let kkmap = match Ordmap.get prod_lhs_index state_lhs_symbol_indexes with
            | None -> kkmap
            | Some symbol_indexes -> begin
                kernels_of_leftmost state prod_lhs_index symbol_indexes
                |> Ordmap.union ~f:(fun _k kernel0 kernel1 -> Lr1Itemset.union kernel0 kernel1) kkmap
              end
            in
            cached := Ordmap.amend state_index ~f:(fun prod_lhs_indexes_opt ->
              match prod_lhs_indexes_opt with
              | None -> not_reached ()
              | Some prod_lhs_indexes -> Some (Ordset.insert prod_lhs_index prod_lhs_indexes)
            ) !cached;
            cache := Ordmap.update_hlt ~k:state_index ~v:kkmap !cache;
(*
            File.Fmt.stderr
            |> Fmt.fmt "XXX B state_index=" |> State.Index.pp state_index
            |> Fmt.fmt "; kkmap="
            |> Ordmap.pp (fun _lr1itemset formatter -> formatter) kkmap
            |> Fmt.fmt "\n"
            |> ignore;
*)
            kkmap
          end
      end
  in
(*
  File.Fmt.stderr
  |> Fmt.fmt "XXX state_index=" |> State.Index.pp state_index
  |> Fmt.fmt "; kkmap="
  |> Ordmap.pp (fun _lr1itemset formatter -> formatter) kkmap
  |> Fmt.fmt "\n"
  |> ignore;
*)
  let k = K.init ~prod_lhs_index ~symbol_index in
  let kernel = match Ordmap.get k kkmap with
  | Some kernel -> begin
(*
      File.Fmt.stderr
      |> Fmt.fmt "XXX Hit: state_index=" |> State.Index.pp state_index
      |> Fmt.fmt "; k=" |> K.pp k
      |> Fmt.fmt "\n"
      |> ignore;
*)
      kernel
    end
  | None -> begin
(*
      File.Fmt.stderr
      |> Fmt.fmt "XXX ---> Miss: state_index=" |> State.Index.pp state_index
      |> Fmt.fmt "; k=" |> K.pp k
      |> Fmt.fmt "\n"
      |> ignore;
*)
      Lr1Itemset.empty
    end
  in
(*
  let xxx_kernel = kernel_of_leftmost state symbol_index prod_lhs_index in
  let () = match Lr1Itemset.(kernel = xxx_kernel) with
  | true -> ()
  | false -> begin
(*
      File.Fmt.stderr
      |> Fmt.fmt "XXX kernel length=" |> Uns.pp (Lr1Itemset.length kernel)
      |> Fmt.fmt "; xxx_kernel length=" |> Uns.pp (Lr1Itemset.length xxx_kernel)
      |> Fmt.fmt "\n"
      |> ignore;
*)
    end
  in
  assert Lr1Itemset.(kernel = xxx_kernel);
*)
  kernel

let kernel_of_rightmost state prod symbol_index =
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

let kernel_of_prod state symbol_index prod =
  match Prod.(prod.rhs_indexes) with
  | [||] -> (* ε production, always associated with an added (non-kernel) item. *)
    kernel_of_leftmost state prod symbol_index
  | _ -> kernel_of_rightmost state prod symbol_index

let kernel_of_prod_index prods state symbol_index prod_index =
  let prod = Prods.prod_of_prod_index prod_index prods in
  kernel_of_prod state symbol_index prod

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

let of_conflict_state ~resolve symbols prods conflict_state =
  let traces = Attribs.fold
      ~init:(Ordmap.empty (module TraceKey))
      ~f:(fun traces {symbol_index; conflict; contrib; _} ->
        Ordset.fold ~init:traces ~f:(fun traces prod_index ->
          let action = State.Action.Reduce prod_index in
          let tracekey = TraceKey.init ~symbol_index ~conflict ~action in
          let lr1itemset = kernel_of_prod_index prods conflict_state symbol_index prod_index in
          let traceval =
            TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset:Lr1Itemset.empty in
          Ordmap.amend tracekey ~f:(fun kernel_isuccs_opt ->
            match kernel_isuccs_opt with
            | None -> Some traceval
            | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
          ) traces
        ) (Contrib.reduces contrib)
      ) (State.conflict_attribs ~resolve symbols prods conflict_state) in
  assert (not (Ordmap.is_empty traces));
  let kernel_attribs = compute_kernel_attribs conflict_state traces in
  {
    conflict_state;
    isucc=conflict_state;
    state=conflict_state;
    traces;
    kernel_attribs;
  }

let of_ipred state {conflict_state; state=isucc; traces=isucc_traces; _} =
  (* Create traces incrementally derived from those in `isucc_traces`. Some traces may terminate at
   * the isucc state; others may continue or even lead to forks. *)
  let traces = Ordmap.fold ~init:(Ordmap.empty (module TraceKey))
    ~f:(fun traces (TraceKey.{symbol_index; action; _} as tracekey, isucc_traceval) ->
      match action with
      | State.Action.ShiftPrefix _
      | ShiftAccept _ -> not_reached ()
      | Reduce _ -> begin
          TraceVal.fold ~init:traces
            ~f:(fun traces
              (Lr1Item.{lr0item=Lr0Item.{prod; dot=isucc_dot}; _}, _isucc_isucc_lr1itemset) ->
              match isucc_dot with
              | 0L -> (* The lane trace terminates at an attribution to the isucc's lr1item. *)
                traces
              | _ -> begin
                  let dot = pred isucc_dot in
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
                    traces
                  | Some lr1item -> begin
                      let lr1itemset = match dot with
                        | 0L -> begin
                            (* Search for kernel items that have the item's LHS symbol just past
                             * their dots and `symbol_index` in their follow sets. *)
                            let kernel = kernel_of_leftmost state prod symbol_index in
                            match Lr1Itemset.is_empty kernel with
                            | true ->
                              (* Contributing state. The trace source is an added item. Attributable
                               * to all lanes leading to this state. *)
                              Lr1Itemset.singleton lr1item
                            | false ->
                              (* Interstitial state. The trace source is one or more kernel items.
                              *)
                              kernel
                          end
                        | _ -> (* Interstitial state. The trace source is a kernel item. *)
                          Lr1Itemset.singleton lr1item
                      in
                      let isucc_lr1itemset = TraceVal.lr1itemset isucc_traceval in
                      let traceval = TraceVal.init symbol_index ~lr1itemset ~isucc_lr1itemset in
                      Ordmap.amend tracekey ~f:(fun traceval_opt ->
                        match traceval_opt with
                        | None -> Some traceval
                        | Some traceval_existing -> Some (TraceVal.union traceval traceval_existing)
                      ) traces
                    end
                end
            ) isucc_traceval
        end
    ) isucc_traces
  in
  let kernel_attribs = compute_kernel_attribs conflict_state traces in
  {
    conflict_state;
    isucc;
    state;
    traces;
    kernel_attribs
  }
