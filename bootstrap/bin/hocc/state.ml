open Basis
open Basis.Rudiments

module Index = StateIndex

module Action = struct
  module T = struct
    type t =
      | ShiftPrefix of Index.t
      | ShiftAccept of Index.t
      | Reduce of Prod.Index.t

    let hash_fold t state =
      match t with
      | ShiftPrefix index -> state |> Uns.hash_fold 0L |> Index.hash_fold index
      | ShiftAccept index -> state |> Uns.hash_fold 1L |> Index.hash_fold index
      | Reduce prod_index -> state |> Uns.hash_fold 2L |> Prod.Index.hash_fold prod_index

    let cmp t0 t1 =
      let open Cmp in
      match t0, t1 with
      | ShiftPrefix _, ShiftAccept _
      | ShiftPrefix _, Reduce _
      | ShiftAccept _, Reduce _
        -> Lt
      | ShiftPrefix i0, ShiftPrefix i1
      | ShiftAccept i0, ShiftAccept i1
        -> Index.cmp i0 i1
      | Reduce i0, Reduce i1
        -> Prod.Index.cmp i0 i1
      | ShiftAccept _, ShiftPrefix _
      | Reduce _, ShiftPrefix _
      | Reduce _, ShiftAccept _
        -> Gt

    let pp t formatter =
      match t with
      | ShiftPrefix index -> formatter |> Fmt.fmt "ShiftPrefix " |> Index.pp index
      | ShiftAccept index -> formatter |> Fmt.fmt "ShiftAccept " |> Index.pp index
      | Reduce prod_index -> formatter |> Fmt.fmt "Reduce " |> Prod.Index.pp prod_index

    let pp_hr symbols prods t formatter =
      match t with
      | ShiftPrefix index -> formatter |> Fmt.fmt "ShiftPrefix " |> Index.pp index
      | ShiftAccept index -> formatter |> Fmt.fmt "ShiftAccept " |> Index.pp index
      | Reduce prod_index -> begin
          let prod = Prods.prod_of_prod_index prod_index prods in
          formatter
          |> Fmt.fmt "Reduce "
          |> Symbols.pp_prod_hr prod symbols
        end
  end
  include T
  include Identifiable.Make(T)

  let reindex state_index_map = function
    | ShiftPrefix state_index ->
      ShiftPrefix (StateIndexMap.reindexed_state_index state_index state_index_map)
    | ShiftAccept state_index ->
      ShiftAccept (StateIndexMap.reindexed_state_index state_index state_index_map)
    | Reduce _ as reduce -> reduce
end

module T = struct
  type t = {
    statenub: StateNub.t;
    resolvers: Resolvers.t;
    actions:
      (Symbol.Index.t, (Action.t, Action.cmper_witness) Ordset.t, Symbol.Index.cmper_witness)
        Ordmap.t;
    gotos: (Symbol.Index.t, Index.t, Symbol.Index.cmper_witness) Ordmap.t;
  }

  let hash_fold {statenub; _} state =
    state |> StateNub.hash_fold statenub

  let cmp {statenub=s0; _} {statenub=s1; _} =
    StateNub.cmp s0 s1

  let pp {statenub; resolvers; actions; gotos} formatter =
    formatter
    |> Fmt.fmt "{statenub=" |> StateNub.pp statenub
    |> Fmt.fmt "; resolvers=" |> Resolvers.pp resolvers
    |> Fmt.fmt "; actions=" |> Ordmap.pp Ordset.pp actions
    |> Fmt.fmt "; gotos=" |> Ordmap.pp Index.pp gotos
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let actions symbols isocores ~gotonub_of_statenub_goto
    (StateNub.{lr1itemsetclosure; _} as statenub) =
  let actions_insert symbol_index action actions = begin
    Ordmap.amend symbol_index ~f:(fun action_set_opt ->
      let action_set' = match action_set_opt with
        | None -> Ordset.singleton (module Action) action
        | Some action_set -> Ordset.insert action action_set
      in
      Some action_set'
    ) actions
  end in
  let token_gotos = Lr1ItemsetClosure.token_gotos symbols lr1itemsetclosure in
  Lr1ItemsetClosure.fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun actions {lr0item={prod={index; rhs_indexes; _}; dot}; follow} ->
      match Uns.(<) dot (Array.length rhs_indexes) with
      (* X :: a·Ab *)
      | true -> begin
          let symbol_index = Array.get dot rhs_indexes in
          let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
          match Symbol.is_token symbol with
          | false -> actions
          | true -> begin
              let goto = Ordmap.get_hlt symbol_index token_gotos in
              let gotonub = gotonub_of_statenub_goto statenub goto in
              let goto_state_index = Isocores.get_hlt gotonub isocores |> StateNub.index in
              let action = match Lr1Itemset.is_accept goto with
                | false -> Action.ShiftPrefix goto_state_index
                | true -> Action.ShiftAccept goto_state_index
              in
              actions_insert symbol_index action actions
            end
        end
      (* X ::= a· *)
      | false -> begin
          Bitset.fold ~init:actions ~f:(fun actions symbol_index ->
            let action = Action.Reduce index in
            actions_insert symbol_index action actions
          ) follow
        end
    ) lr1itemsetclosure

let resolve_symbol precs symbols prods symbol_index action_set =
  let prec_set_of_action precs symbols prods symbol_index action = begin
    let open Action in
    let prec_opt = match action with
      | ShiftPrefix _
      | ShiftAccept _ ->
        (match Symbols.symbol_of_symbol_index symbol_index symbols with Symbol.{prec; _} -> prec)
      | Reduce prod_index ->
        (match Prods.prod_of_prod_index prod_index prods with Prod.{prec; _} -> prec)
    in
    match prec_opt with
    | None -> None
    | Some Prec.{prec_set_index; _} -> Some (Precs.prec_set_of_prec_index prec_set_index precs)
  end in
  let assoc_of_action precs symbols prods symbol_index action = begin
    match prec_set_of_action precs symbols prods symbol_index action with
    | None -> None
    | Some PrecSet.{assoc; _} -> assoc
  end in
  match Ordset.length action_set with
  | 1L -> Resolvers.empty, action_set
  | _ -> begin
      (* Compute the subset of actions with maximal precedence, if any. Disjoint precedences are
       * incomparable, i.e. there is no maximal precedence in the presence of disjoint
       * precedences. *)
      let max_prec_action_set =
        Ordset.fold_until ~init:(Ordset.empty (module Action))
          ~f:(fun max_prec_action_set action ->
            match Ordset.is_empty max_prec_action_set with
            | true -> Ordset.singleton (module Action) action, false
            | false -> begin
                let max_prec_set = prec_set_of_action precs symbols prods symbol_index
                    (Ordset.choose_hlt max_prec_action_set) in
                let action_prec_set = prec_set_of_action precs symbols prods symbol_index action in
                match max_prec_set, action_prec_set with
                | None, _
                | _, None -> begin
                    (* Disjoint lack of precedence(s). *)
                    Ordset.empty (module Action), true
                  end
                | Some max_prec_set, Some action_prec_set -> begin
                    match Uns.(=) max_prec_set.index action_prec_set.index with
                    | false -> begin
                        match Bitset.mem max_prec_set.index action_prec_set.doms with
                        | false -> begin
                            match Bitset.mem action_prec_set.index max_prec_set.doms
                            with
                            | false -> begin
                                (* Disjoint precedence; no conflict resolution possible. *)
                                Ordset.empty (module Action), true
                              end
                            | true -> begin
                                (* Action's precedence exceeds current maximal precedence. Replace
                                 * dominated set with the singleton set containing action. *)
                                Ordset.singleton (module Action) action, false
                              end
                          end
                        | true -> begin
                            (* Current maximal precedence dominates action's precedence. *)
                            max_prec_action_set, false
                          end
                      end
                    | true -> begin
                        (* Precedence equal to current maximal precedence. *)
                        Ordset.insert action max_prec_action_set, false
                      end
                  end
              end
          ) action_set
      in
      let resolvers = match Ordset.is_empty max_prec_action_set with
        | true -> Resolvers.empty
        | false -> begin
            Ordset.fold ~init:Resolvers.empty ~f:(fun resolvers action ->
              let open Action in
              match action with
              | ShiftPrefix _
              | ShiftAccept _ -> Resolvers.use_token_prec symbol_index resolvers
              | Reduce prod_index -> Resolvers.use_prod_prec prod_index resolvers
            ) action_set
          end
      in
      match Ordset.length max_prec_action_set with
      | 0L -> Resolvers.empty, action_set
      | 1L -> resolvers, max_prec_action_set
      | _ -> begin
          (* Determine whether the subset of actions with maximal precedence has homogeneous
           * associativity. *)
          let assoc = assoc_of_action precs symbols prods symbol_index
              (Ordset.choose_hlt max_prec_action_set) in
          let homogeneous = Ordset.fold_until ~init:true ~f:(fun _ action ->
            let action_assoc = assoc_of_action precs symbols prods symbol_index action in
            match Cmp.is_eq (Option.cmp Assoc.cmp assoc action_assoc) with
            | false -> false, true
            | true -> true, false
          ) max_prec_action_set in
          match homogeneous with
          | false -> Resolvers.empty, action_set
          | true -> begin
              let resolvers = match Uns.(Ordset.length max_prec_action_set > 1L) with
                | false -> resolvers
                | true -> begin
                    Ordset.fold ~init:resolvers ~f:(fun resolvers action ->
                      let open Action in
                      let prec_opt = match action with
                        | ShiftPrefix _
                        | ShiftAccept _ -> begin
                            match Symbols.symbol_of_symbol_index symbol_index symbols with
                              Symbol.{prec; _} -> prec
                          end
                        | Reduce prod_index -> begin
                            match Prods.prod_of_prod_index prod_index prods with
                              Prod.{prec; _} -> prec
                          end
                      in
                      let Prec.{prec_set_index; _} = Option.value_hlt prec_opt in
                      Resolvers.use_assoc prec_set_index resolvers
                    ) max_prec_action_set
                  end
              in
              match assoc with
              | None -> begin
                  (* Resolve a singleton. *)
                  match Ordset.length max_prec_action_set with
                  | 1L -> resolvers, max_prec_action_set
                  | _ -> Resolvers.empty, action_set
                end
              | Some Left -> begin
                  (* Resolve a single reduce action. *)
                  let reduce_action_set = Ordset.fold_until
                      ~init:(Ordset.empty (module Action))
                      ~f:(fun reduce_action_set action ->
                        let open Action in
                        match action with
                        | ShiftPrefix _
                        | ShiftAccept _ -> reduce_action_set, false
                        | Reduce _ -> begin
                            match Ordset.is_empty reduce_action_set with
                            | false -> Ordset.empty (module Action), true
                            | true -> Ordset.singleton (module Action) action, false
                          end
                      ) max_prec_action_set in
                  match Ordset.length reduce_action_set with
                  | 1L -> resolvers, reduce_action_set
                  | _ -> Resolvers.empty, action_set
                end
              | Some Right -> begin
                  (* Resolve a (single) shift action. *)
                  let shift_action_set = Ordset.fold_until
                      ~init:(Ordset.empty (module Action))
                      ~f:(fun shift_action_set action ->
                        let open Action in
                        match action with
                        | ShiftPrefix _
                        | ShiftAccept _ -> Ordset.singleton (module Action) action, true
                        | Reduce _ -> shift_action_set, false
                      ) max_prec_action_set in
                  match Ordset.length shift_action_set with
                  | 1L -> resolvers, shift_action_set
                  | _ -> Resolvers.empty, action_set
                end
              | Some Nonassoc -> resolvers, (Ordset.empty (module Action))
            end
        end
    end

let resolve_actions precs symbols prods actions =
  Ordmap.fold ~init:(Resolvers.empty, Ordmap.empty (module Symbol.Index))
    ~f:(fun (resolvers, actions) (symbol_index, action_set) ->
      let symbol_resolvers, action_set' =
        resolve_symbol precs symbols prods symbol_index action_set in
      let resolvers = Resolvers.union symbol_resolvers resolvers in
      (* Nonassoc can cause empty action sets; drop them from actions. *)
      let actions = match Ordset.is_empty action_set' with
        | true -> actions
        | false -> Ordmap.insert_hlt ~k:symbol_index ~v:action_set' actions
      in
      resolvers, actions
    ) actions

let init ~resolve precs symbols prods isocores ~gotonub_of_statenub_goto
    (StateNub.{lr1itemsetclosure; _} as statenub) =
  let actions_raw = actions symbols isocores ~gotonub_of_statenub_goto statenub in
  let resolvers, actions = match resolve with
    | false -> Resolvers.empty, actions_raw
    | true -> resolve_actions precs symbols prods actions_raw
  in
  let gotos =
    Lr1ItemsetClosure.nonterm_gotos symbols lr1itemsetclosure
    |> Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index)) ~f:(fun gotos (nonterm_index, goto) ->
      let gotonub = gotonub_of_statenub_goto statenub goto in
      Ordmap.insert_hlt ~k:nonterm_index ~v:(Isocores.get_hlt gotonub isocores |> StateNub.index)
        gotos
    )
  in
  {statenub; resolvers; actions; gotos}

let normalize_state_index remergeable_index_map state_index =
  Ordmap.get state_index remergeable_index_map
  |> Option.value ~default:state_index

let normalize_action_set remergeable_index_map action_set =
  Ordset.fold ~init:(Ordset.empty (module Action)) ~f:(fun action_set' action ->
    let open Action in
    let action' = match action with
      | ShiftPrefix index -> ShiftPrefix (normalize_state_index remergeable_index_map index)
      | ShiftAccept index -> ShiftAccept (normalize_state_index remergeable_index_map index)
      | Reduce _ as reduce -> reduce
    in
    Ordset.insert action' action_set'
  ) action_set

let remerge symbols remergeable_index_map
    {statenub=sn0; resolvers=r0; actions=a0; gotos=g0}
    {statenub=sn1; resolvers=r1; actions=a1; gotos=g1} =
  let statenub = StateNub.remerge symbols sn0 sn1 in
  let resolvers = Resolvers.union r0 r1 in
  let actions =
    Ordmap.fold ~init:a1 ~f:(fun actions (symbol_index, action_set0) ->
      let action_set0 = normalize_action_set remergeable_index_map action_set0 in
      Ordmap.amend symbol_index ~f:(fun actions_opt ->
        match actions_opt with
        | None -> Some action_set0
        | Some action_set1 -> Some (Ordset.union action_set0 action_set1)
      ) actions
    ) a0
  in
  let gotos = Ordmap.fold ~init:g1 ~f:(fun gotos (symbol_index, goto) ->
    Ordmap.insert ~k:symbol_index ~v:(normalize_state_index remergeable_index_map goto) gotos
  ) g0 in
  {statenub; resolvers; actions; gotos}

let reindex state_index_map reachable_action_symbols_opt ({statenub; actions; gotos; _} as t) =
  let reachable_action_symbols = match reachable_action_symbols_opt with
    | None -> Ordmap.fold ~init:(Ordset.empty (module Symbol.Index))
      ~f:(fun reachable_action_symbols (symbol_index, _actions) ->
        Ordset.insert symbol_index reachable_action_symbols
      ) actions
    | Some reachable_action_symbols -> reachable_action_symbols
  in
  let statenub = StateNub.reindex state_index_map statenub in
  let actions = Ordmap.filter_map ~f:(fun (symbol_index, actions) ->
    match Ordset.mem symbol_index reachable_action_symbols with
    | false -> None
    | true -> begin
        Some (Ordset.fold ~init:(Ordset.empty (module Action))
          ~f:(fun reindexed_actions action ->
            let reindexed_action = Action.reindex state_index_map action in
            Ordset.insert reindexed_action reindexed_actions
          ) actions )
      end
  ) actions in
  let gotos = Ordmap.filter_map ~f:(fun (_symbol_index, statenub_index) ->
    StateIndexMap.reindexed_state_index_opt statenub_index state_index_map
  ) gotos in
  {t with statenub; actions; gotos}

let index {statenub={lr1itemsetclosure={index; _}; _}; _} =
  index

let is_start {statenub={lr1itemsetclosure={kernel; _}; _}; _} =
  Lr1Itemset.is_start kernel

let start_symbol_index {statenub={lr1itemsetclosure={kernel; _}; _}; _} =
  Lr1Itemset.start_symbol_index kernel

let has_pseudo_end_conflict {actions; _} =
  match Ordmap.mem Symbol.pseudo_end.index actions, Ordmap.length actions with
  | false, _
  | true, 1L (* A state with only an action on ⊥ is conflict-free. *)
    -> false
  | true, _ -> true

let conflicts_alist ~resolve precs symbols prods {actions; _} =
  Ordmap.fold ~init:[] ~f:(fun symbol_index_actions (symbol_index, actions) ->
    match Ordset.length actions with
    | 0L -> not_reached ()
    | 1L -> symbol_index_actions
    | _ -> (symbol_index, actions) :: symbol_index_actions
  ) actions
  |> List.fold ~init:[] ~f:(fun symbol_index_conflict (symbol_index, actions) ->
    let conflict = Ordset.fold ~init:Contrib.empty ~f:(fun conflict action ->
      let open Action in
      match action with
      | ShiftPrefix _
      | ShiftAccept _ -> Contrib.(union shift conflict)
      | Reduce prod_index -> Contrib.(union (init_reduce prod_index) conflict)
    ) actions in
    match resolve with
    | false -> (symbol_index, conflict) :: symbol_index_conflict
    | true -> begin
        (* Conflicts which resolve to a shift action can be ignored because state merging cannot
         * affect resolutions, but all others must be traced. *)
        let resolved = Contrib.resolve precs symbols prods symbol_index conflict in
        match Contrib.(equal resolved shift) with
        | true -> symbol_index_conflict
        | false -> (symbol_index, conflict) :: symbol_index_conflict
      end
  )

let has_conflict_attribs ~resolve precs symbols prods t =
  conflicts_alist ~resolve precs symbols prods t
  |> List.is_empty
  |> Bool.not

let conflict_attribs ~resolve precs symbols prods t =
  let conflict_state_index = index t in
  conflicts_alist ~resolve precs symbols prods t
  |> List.fold ~init:Attribs.empty
    ~f:(fun attribs (symbol_index, conflict) ->
      (* This function is only called by `LaneCtx.of_conflict_state`, for which case
       * `isucc_lr1itemset` is always empty, because there is no isucc state for the conflict state.
      *)
      let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
          ~isucc_lr1itemset:Lr1Itemset.empty ~contrib:conflict in
      Attribs.insert attrib attribs
    )

let conflicts ?(filter_pseudo_end=true) ({actions; _} as t) =
  Ordset.union
    (match (not filter_pseudo_end) && has_pseudo_end_conflict t with
        | false -> Ordset.empty (module Symbol.Index)
        | true -> Ordset.singleton (module Symbol.Index) Symbol.pseudo_end.index)
    (Ordmap.fold ~init:(Ordset.empty (module Symbol.Index))
        ~f:(fun symbol_indexes (symbol_index, action_set) ->
          match (Ordset.length action_set) with
          | 1L -> symbol_indexes
          | _ -> Ordset.insert symbol_index symbol_indexes
        ) actions)
  |> Ordset.length

let sr_conflicts {actions; _} =
  Ordmap.count ~f:(fun (_, action_set) ->
    let shifts, reduces = Ordset.fold_until ~init:(0L, 0L) ~f:(fun (shifts, reduces) action ->
      let open Action in
      let shifts', reduces' = match action with
        | ShiftPrefix _
        | ShiftAccept _ -> succ shifts, reduces
        | Reduce _ -> shifts, succ reduces
      in
      (shifts', reduces'), Uns.(shifts' > 0L && reduces' > 0L)
    ) action_set
    in
    Uns.(shifts > 0L && reduces > 0L)
  ) actions

let rr_conflicts {actions; _} =
  Ordmap.count ~f:(fun (_, action_set) ->
    let reduces = Ordset.fold_until ~init:0L ~f:(fun reduces action ->
      let open Action in
      let reduces' = match action with
        | ShiftPrefix _
        | ShiftAccept _ -> reduces
        | Reduce _ -> succ reduces
      in
      reduces', Uns.(reduces' > 1L)
    ) action_set
    in
    Uns.(reduces > 1L)
  ) actions
