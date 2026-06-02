open Basis
open! Basis.Rudiments

type remergeable =
  | NotRemergeable of Remergeables.spines (* Spines' state nubs are pairwise distinct. *)
  | Remergeable

let bool_of_remergeable remergeable =
  match remergeable with
  | NotRemergeable _ -> false
  | Remergeable -> true

let remergeable_action_sets states remergeables frontiers spines action_set0 action_set1 =
  let open State.Action in
  State.ActionSet.fold2_until ~init:(remergeables, frontiers, Remergeable)
    ~f:(fun (remergeables, frontiers, _remergeable) kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some (ShiftPrefix index0), Some (ShiftPrefix index1)
      | Some (ShiftAccept index0), Some (ShiftAccept index1) -> begin
          match State.Index.(index0 = index1) with
          | true -> (remergeables, frontiers, Remergeable), false
          | false -> begin
              let State.{statenub=statenub0; _} = Array.get index0 states in
              let State.{statenub=statenub1; _} = Array.get index1 states in
              let spines = (statenub0, statenub1) :: spines in
              match Remergeables.rel statenub0 statenub1 remergeables with
              | Unknown -> begin
                  let frontiers = spines :: frontiers in
                  (remergeables, frontiers, Remergeable), false
                end
              | Distinct -> (remergeables, frontiers, NotRemergeable spines), true
              | Mergeable -> (remergeables, frontiers, Remergeable), false
            end
        end
      | Some (ShiftPrefix _), Some (ShiftAccept _)
      | Some (ShiftAccept _), Some (ShiftPrefix _)
        -> not_reached ()
      | Some _, Some _ (* fold2 guarantees equality for reduces. *)
      | Some (ShiftPrefix _|ShiftAccept _), None
      | None, Some (ShiftPrefix _|ShiftAccept _)
        -> (remergeables, frontiers, Remergeable), false
      | Some (Reduce _), None (* Reduce in one set but not the other. *)
      | None, Some (Reduce _)
        -> (remergeables, frontiers, NotRemergeable spines), true
      | None, None -> not_reached ()
    ) action_set0 action_set1

let remergeable_actions states remergeables frontiers spines =
  let reduces_only action_set = begin
    let open State.Action in
    State.ActionSet.for_all
      ~f:(fun action ->
        match action with
        | ShiftPrefix _
        | ShiftAccept _
          -> false
        | Reduce _
          -> true
      ) action_set
  end in
  let statenub0, statenub1 = match spines with
    | [] -> not_reached ()
    | statenub_pair :: _ -> statenub_pair
  in
  let index0 = StateNub.index statenub0 in
  let index1 = StateNub.index statenub1 in
  let State.{actions=a0; _} = Array.get index0 states in
  let State.{actions=a1; _} = Array.get index1 states in
  Ordmap.fold2_until ~init:(remergeables, frontiers, Remergeable)
    ~f:(fun (remergeables, frontiers, _remergeable) kv0_opt kv1_opt ->
      let remergeables, frontiers, remergeable = match kv0_opt, kv1_opt with
        | Some (_, action_set0), Some (_, action_set1)
          -> remergeable_action_sets states remergeables frontiers spines action_set0 action_set1
        | Some (symbol_index, action_set), None
        | None, Some (symbol_index, action_set)
          -> begin
              (* All states in the mergeable set must either have an empty action set or identical
               * nonempty action set. Note that these are post-conflict-resolution action sets; were
               * they pre-conflict-resolution action sets (i.e. conflict resolution to be performed
               * after remerging), the equivalent remergeability test would require identical
               * dominant actions (or identical [possibly unresolvable] action sets). *)
              match reduces_only action_set with
              | false -> remergeables, frontiers, NotRemergeable spines
              | true -> begin
                  let remergeable =
                    remergeables
                    |> Remergeables.mergeable_set statenub0
                    |> Ordset.fold_until ~init:Remergeable
                      ~f:(fun _remergeable statenub ->
                        let index = StateNub.index statenub in
                        let State.{actions; _} = Array.get index states in
                        match Ordmap.get symbol_index actions with
                        | None -> Remergeable, false
                        | Some mergeable_action_set -> begin
                            match State.ActionSet.equal action_set mergeable_action_set with
                            | false -> NotRemergeable spines, true
                            | true -> Remergeable, false
                          end
                      )
                  in
                  remergeables, frontiers, remergeable
                end
            end
        | None, None
          -> not_reached ()
      in
      (remergeables, frontiers, remergeable), (not (bool_of_remergeable remergeable))
    ) a0 a1

let remergeable_gotos states remergeables frontiers spines =
  let statenub0, statenub1 = match spines with
    | [] -> not_reached ()
    | statenub_pair :: _ -> statenub_pair
  in
  let index0 = StateNub.index statenub0 in
  let index1 = StateNub.index statenub1 in
  let State.{gotos=g0; _} = Array.get index0 states in
  let State.{gotos=g1; _} = Array.get index1 states in
  Ordmap.fold2_until ~init:(remergeables, frontiers, Remergeable)
    ~f:(fun (remergeables, frontiers, _remergeable) kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some (_, index0), Some (_, index1) -> begin
          let remergeables, frontiers, remergeable = match State.Index.(index0 = index1) with
            | true -> remergeables, frontiers, Remergeable
            | false -> begin
                let State.{statenub=statenub0; _} = Array.get index0 states in
                let State.{statenub=statenub1; _} = Array.get index1 states in
                let spines = (statenub0, statenub1) :: spines in
                match Remergeables.rel statenub0 statenub1 remergeables with
                | Unknown -> begin
                    let frontiers = spines :: frontiers in
                    remergeables, frontiers, Remergeable
                  end
                | Distinct -> remergeables, frontiers, NotRemergeable spines
                | Mergeable -> remergeables, frontiers, Remergeable
              end
          in
          (remergeables, frontiers, remergeable), (not (bool_of_remergeable remergeable))
        end
      | Some (symbol_index, index0), None
      | None, Some (symbol_index, index0) -> begin
          (* All states in the mergeable set must either lack a goto or be remergeable with the
           * other gotos. Testing against one other goto suffices due to transitivity. *)
          let State.{statenub=statenub0; _} = Array.get index0 states in
          let remergeables, frontiers, remergeable =
            remergeables
            |> Remergeables.mergeable_set statenub0
            |> Ordset.fold_until ~init:(remergeables, frontiers, Remergeable)
              ~f:(fun (remergeables, frontiers, _remergeable) statenub ->
                let index = StateNub.index statenub in
                let State.{gotos; _} = Array.get index states in
                match Ordmap.get symbol_index gotos with
                | None -> (remergeables, frontiers, Remergeable), false
                | Some index1 -> begin
                    let State.{statenub=statenub1; _} = Array.get index1 states in
                    let spines = (statenub0, statenub1) :: spines in
                    let remergeables, frontiers, remergeable =
                      match Remergeables.rel statenub0 statenub1 remergeables with
                      | Unknown -> begin
                          let frontiers = spines :: frontiers in
                          remergeables, frontiers, Remergeable
                        end
                      | Distinct -> remergeables, frontiers, NotRemergeable spines
                      | Mergeable -> remergeables, frontiers, Remergeable
                    in
                    (remergeables, frontiers, remergeable), true
                  end
              )
          in
          (remergeables, frontiers, remergeable), (not (bool_of_remergeable remergeable))
        end
      | None, None -> not_reached ()
    ) g0 g1

let remergeable_spines states remergeables frontiers spines =
  let statenub0, statenub1 = match spines with
    | [] -> not_reached ()
    | statenub_pair :: _ -> statenub_pair
  in
  match Remergeables.rel statenub0 statenub1 remergeables with
  | Unknown -> begin
      let remergeables = Remergeables.expand statenub0 statenub1 remergeables in
      let remergeables, frontiers, remergeable =
        remergeable_actions states remergeables frontiers spines in
      match remergeable with
      | NotRemergeable _ -> remergeables, frontiers, remergeable
      | Remergeable -> remergeable_gotos states remergeables frontiers spines
    end
  | Distinct -> not_reached ()
  | Mergeable -> remergeables, frontiers, Remergeable

let remergeable_statenubs states remergeables statenub0 statenub1 =
  (* Perform breadth-first search (BFS) by ratcheting frontiers, i.e. explore `frontiers_current`,
   * then promote `frontiers_next` to `frontiers_current`, until a distinction is discovered or all
   * frontiers are explored.
   *
   * BFS is a better choice than depth-first search (DFS) because it is less susceptible to
   * squandering time exploring long paths in a heavily interconnected automaton when there is often
   * a distinction to be found along a much shorter path. *)
  let rec inner states remergeables ~frontiers_next ~frontiers_current = begin
    match frontiers_current with
    | [] -> begin
        match frontiers_next with
        | [] -> remergeables, Remergeable
        | _ :: _ -> inner states remergeables ~frontiers_next:[] ~frontiers_current:frontiers_next
      end
    | spines :: frontiers_current -> begin
        let remergeables, frontiers_next, remergeable =
          remergeable_spines states remergeables frontiers_next spines in
        match remergeable with
        | NotRemergeable _ -> remergeables, remergeable
        | Remergeable -> inner states remergeables ~frontiers_next ~frontiers_current
      end
  end in
  let spines = (statenub0, statenub1) :: [] in
  let frontiers_current = [spines] in
  inner states remergeables ~frontiers_next:[] ~frontiers_current

let remergeable_search io isocores states =
  let io =
    io.log
    |> Fmt.fmt "hocc: Searching for remergeable states"
    |> Io.with_log io
  in
  (* Fold over all non-singleton isocoric state nub sets in `isocores` to search for remergeable
   * pairs within each set. Process sets in order of increasing cardinality based on the heuristic
   * that smaller sets will be involved in less complex splits. Determining the remergeability of
   * small subgraphs early on reduces the search complexity for large adjacent/containing subgraphs
   * later on, whereas the converse — searching large subgraphs first — is of negligible benefit
   * to later searches of small subgraphs. *)
  let nmergeable, max_mergeable, remergeables =
    Isocores.fold_non_singleton_isocore_sets ~init:(0L, 0L, Remergeables.empty)
      ~f:(fun (nmergeable, max_mergeable, remergeables) isocore_set0 ->
        let isocore_set_length = Ordset.length isocore_set0 in
        match isocore_set_length with
        | 0L -> not_reached ()
        | 1L -> nmergeable, max_mergeable, remergeables
        | _ -> begin
            let max_mergeable = max_mergeable + (pred isocore_set_length) in
            (* Test all n-choose-2 pairings for remergeability. *)
            let nmergeable, remergeables, _isocore_set1 =
              Ordset.fold ~init:(nmergeable, remergeables, isocore_set0)
                ~f:(fun (nmergeable, remergeables, isocore_set1) statenub0 ->
                  let isocore_set1 = Ordset.remove statenub0 isocore_set1 in
                  let nmergeable, remergeables =
                    Ordset.fold ~init:(nmergeable, remergeables)
                      ~f:(fun (nmergeable, remergeables) statenub1 ->
                        let nmergeable, remergeables =
                          (* `Distinct`/`Mergeable` indicates that an earlier search starting at
                           * predecessors transitively determined this pair's relationship. *)
                          match Remergeables.rel statenub0 statenub1 remergeables with
                          | Distinct
                          | Mergeable -> nmergeable, remergeables
                          | Unknown -> begin
                              let remergeables, remergeable =
                                remergeable_statenubs states remergeables statenub0 statenub1 in
                              match remergeable with
                              | NotRemergeable spines ->
                                nmergeable, Remergeables.distinct spines remergeables
                              | Remergeable -> begin
                                  let subgraph_size = Remergeables.subgraph_size remergeables in
                                  nmergeable + subgraph_size, Remergeables.mergeable remergeables
                                end
                            end
                        in
                        nmergeable, remergeables
                      ) isocore_set1
                  in
                  nmergeable, remergeables, isocore_set1
                ) isocore_set0
            in
            nmergeable, max_mergeable, remergeables
          end
      ) isocores in
  let io =
    io.log
    |> Fmt.fmt ": "
    |> Uns.pp nmergeable |> Fmt.fmt "/" |> Uns.pp max_mergeable
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  io, remergeables

let remerge_states io symbols isocores states =
  let io, remergeables = remergeable_search io isocores states in
  let remergeable_index_map = Remergeables.index_map remergeables in
  let nremergeable = Ordmap.length remergeable_index_map in
  match nremergeable with
  | 0L -> begin
      let io = io.log |> Fmt.fmt "hocc: 0 remergeable states\n" |> Io.with_log io in
      io, isocores, states
    end
  | _ -> begin
      let io =
        io.log
        |> Fmt.fmt "hocc: Remerging " |> Uns.pp nremergeable |> Fmt.fmt " LR(1) state"
        |> (fun formatter ->
          match nremergeable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
        )
        |> Fmt.fmt "\n"
        |> Io.with_log io
      in
      let remaining_state_indexes = Range.Uns.fold (0L =:< Array.length states)
        ~init:(Ordset.empty (module State.Index))
        ~f:(fun reachable_state_indexes i ->
          match Ordmap.mem i remergeable_index_map with
          | true -> reachable_state_indexes
          | false -> Ordset.insert i reachable_state_indexes
        ) in
      let nremaining = Ordset.length remaining_state_indexes in
      (* Create a map that reindexes the remaining states. *)
      let state_index_map = StateIndexMap.init ~remaining_state_indexes ~remergeable_index_map
          ~isocores_sn_of_state_index:(fun state_index ->
            let State.{statenub={isocores_sn; _}; _} = Array.get state_index states in
            isocores_sn
          ) in
      (* Remerge and reindex isocores. *)
      let reindexed_isocores =
        Ordmap.fold ~init:isocores
          ~f:(fun remerged_isocores (index0, index1) ->
            assert State.Index.(index0 > index1);
            Isocores.remerge symbols index0 index1 remerged_isocores
          ) remergeable_index_map
        |> Isocores.reindex state_index_map in
      (* Remerge remergeable states. *)
      let remerged_states = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
        ~f:(fun remerged_states (index0, index1) ->
          assert State.Index.(index0 > index1);
          let state0 = Array.get index0 states in
          let state1 = match Ordmap.get index1 remerged_states with
            | None -> Array.get index1 states
            | Some state1 -> state1
          in
          let state1' = State.remerge symbols state0 state1 in
          let remerged_states = Ordmap.upsert ~k:index1 ~v:state1' remerged_states in
          remerged_states
        ) remergeable_index_map in
      let io =
        io.log
        |> Fmt.fmt "hocc: Reindexing " |> Uns.pp nremaining |> Fmt.fmt " LR(1) state"
        |> (fun formatter -> match nremaining with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
        |> Io.with_log io
      in
      (* Create a new set of reindexed states. The states corresponding to `remaining_state_indexes`
       * are in `remerged_states` if remergeable, in `states` otherwise. *)
      let reindexed_states =
        Ordset.fold ~init:(Ordset.empty (module State)) ~f:(fun reindexed_states state_index ->
          let state = match Ordmap.get state_index remerged_states with
            | None -> Array.get state_index states
            | Some state -> state
          in
          let reindexed_state = State.reindex state_index_map None state in
          Ordset.insert reindexed_state reindexed_states
        ) remaining_state_indexes
        |> Ordset.to_array in
      let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
      io, reindexed_isocores, reindexed_states
    end
