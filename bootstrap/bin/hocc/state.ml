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
  end
  include T
  include Identifiable.Make(T)

  let reindex index_map = function
    | ShiftPrefix state_index -> ShiftPrefix (Map.get_hlt state_index index_map)
    | ShiftAccept state_index -> ShiftAccept (Map.get_hlt state_index index_map)
    | Reduce _ as reduce -> reduce
end

module T = struct
  type t = {
    statenub: StateNub.t;
    actions:
      (Symbol.Index.t, (Action.t, Action.cmper_witness) Ordset.t, Symbol.Index.cmper_witness)
        Ordmap.t;
    gotos: (Symbol.Index.t, Lr1ItemsetClosure.Index.t, Symbol.Index.cmper_witness) Ordmap.t;
  }

  let hash_fold {statenub; _} state =
    state |> StateNub.hash_fold statenub

  let cmp {statenub=s0; _} {statenub=s1; _} =
    StateNub.cmp s0 s1

  let pp {statenub; actions; gotos} formatter =
    formatter
    |> Fmt.fmt "{statenub=" |> StateNub.pp statenub
    |> Fmt.fmt "; actions=" |> Ordmap.pp Ordset.pp actions
    |> Fmt.fmt "; gotos=" |> Ordmap.pp Lr1ItemsetClosure.Index.pp gotos
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~resolve symbols prods isocores ~gotonub_of_statenub_goto statenub =
  let actions =
    StateNub.actions symbols statenub
    |> (fun statenub -> match resolve with
      | false -> statenub
      | true -> statenub |> StateNub.resolve symbols prods
    )
    |> Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index))
      ~f:(fun actions (symbol_index, action_set) ->
        let action_set' = Ordset.fold ~init:(Ordset.empty (module Action))
          ~f:(fun action_set action ->
            Ordset.insert (match action with
              | StateNub.Action.ShiftPrefix goto -> begin
                  let gotonub = gotonub_of_statenub_goto statenub goto in
                  Action.ShiftPrefix (Isocores.get_hlt gotonub isocores)
                end
              | StateNub.Action.ShiftAccept goto -> begin
                  let gotonub = gotonub_of_statenub_goto statenub goto in
                  Action.ShiftAccept (Isocores.get_hlt gotonub isocores)
                end
              | StateNub.Action.Reduce prod_index -> Action.Reduce prod_index
            ) action_set
          ) action_set in
        Ordmap.insert ~k:symbol_index ~v:action_set' actions
      )
  in
  let gotos =
    StateNub.gotos symbols statenub
    |> Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index)) ~f:(fun gotos (nonterm_index, goto) ->
      let gotonub = gotonub_of_statenub_goto statenub goto in
      Ordmap.insert_hlt ~k:nonterm_index ~v:(Isocores.get_hlt gotonub isocores) gotos
    )
  in
  {statenub; actions; gotos}

let reindex index_map {statenub; actions; gotos} =
  let statenub = StateNub.reindex index_map statenub in
  let actions = Ordmap.map ~f:(fun (_symbol_index, actions) ->
    Ordset.fold ~init:(Ordset.empty (module Action))
      ~f:(fun reindexed_actions action ->
        let reindexed_action = Action.reindex index_map action in
        Ordset.insert reindexed_action reindexed_actions
      ) actions
  ) actions in
  let gotos = Ordmap.map ~f:(fun (_symbol_index, statenub_index) ->
    Map.get_hlt statenub_index index_map
  ) gotos in
  {statenub; actions; gotos}

let index {statenub={lr1itemsetclosure={index; _}; _}; _} =
  index

let is_start {statenub={lr1itemsetclosure={kernel; _}; _}; _} =
  Lr1Itemset.is_start kernel

let has_pseudo_end_conflict {actions; _} =
  match Ordmap.mem Symbol.pseudo_end.index actions, Ordmap.length actions with
  | false, _
  | true, 1L (* A state with only an action on ⊥ is conflict-free. *)
    -> false
  | true, _ -> true

let conflict_attribs ~resolve symbols prods {actions; _} =
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
        let resolved = Contrib.resolve symbols prods symbol_index conflict in
        match Contrib.mem_shift resolved && (Uns.(=) (Contrib.length resolved) 1L) with
        | true -> symbol_index_conflict
        | false -> (symbol_index, conflict) :: symbol_index_conflict
      end
  )
  |> List.fold ~init:Attribs.empty ~f:(fun attribs (symbol_index, conflict) ->
    (* This function is only called by `LaneCtx.of_conflict_state`, for which case
     * `isucc_lr1itemset` is always empty, because there is no isucc state for the conflict state.
    *)
    let attrib =
      Attrib.init ~symbol_index ~conflict ~isucc_lr1itemset:Lr1Itemset.empty ~contrib:conflict in
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
