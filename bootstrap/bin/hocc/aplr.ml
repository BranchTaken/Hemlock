open Basis
open! Basis.Rudiments

(* Pairwise state nubs comprising the (reversed) path from frontiers to roots. Spines are tracked in
 * case a `Distinct` pair is discovered, in which case the spines are passed to
 * `Remergeables.distinct`. *)
type spines = (StateNub.t * StateNub.t) list

type remergeable =
  | NotRemergeable of spines (* Spines' state nubs are pairwise distinct. *)
  | Remergeable

let bool_of_remergeable remergeable =
  match remergeable with
  | NotRemergeable _ -> false
  | Remergeable -> true

let remergeable_action_set_shifts states remergeables frontiers spines action_set0 action_set1 =
  (* Check whether the sets have equivalent shift actions (or no shift actions). This implementation
   * takes advantage of action comparison order putting shift actions before reduce actions, i.e. if
   * there is a shift action, it is the first set element. *)
  let open State.Action in
  match Ordset.nth 0L action_set0, Ordset.nth 0L action_set1 with
  | ShiftPrefix index0, ShiftPrefix index1
  | ShiftAccept index0, ShiftAccept index1 -> begin
      match State.Index.(index0 = index1) with
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
    end
  | Reduce _, Reduce _ ->
    remergeables, frontiers, Remergeable (* Handled in `remergeable_action_set_reduces`. *)
  | _ -> remergeables, frontiers, NotRemergeable spines (* Shift in one set but not the other. *)

let remergeable_action_set_reduces spines action_set0 action_set1 =
  (* Check whether the sets have equivalent reduce actions. Shift actions are handled above, since
   * they only show up together in fold2 if they shift to the same state index. *)
  let open State.Action in
  Ordset.fold2_until ~init:Remergeable ~f:(fun _remergeable kv0_opt kv1_opt ->
    match kv0_opt, kv1_opt with
    | Some _, Some _ (* fold2 guarantees equality. *)
    | Some (ShiftPrefix _|ShiftAccept _), None (* Handled in `remergeable_action_set_shifts`. *)
    | None, Some (ShiftPrefix _|ShiftAccept _)
      -> Remergeable, false
    | Some (Reduce _), None (* Reduce in one set but not the other. *)
    | None, Some (Reduce _)
      -> NotRemergeable spines, true
    | None, None -> not_reached ()
  ) action_set0 action_set1

let remergeable_action_sets states remergeables frontiers spines action_set0 action_set1 =
  let remergeables, frontiers, remergeable =
    remergeable_action_set_shifts states remergeables frontiers spines action_set0 action_set1 in
  match remergeable with
  | NotRemergeable _ -> remergeables, frontiers, remergeable
  | Remergeable ->
    remergeables, frontiers, remergeable_action_set_reduces spines action_set0 action_set1

let remergeable_actions states remergeables frontiers spines =
  let reduces_only action_set = begin
    let open State.Action in
    Ordset.for_all
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
               * nonempty action set. *)
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
                            match Ordset.equal action_set mergeable_action_set with
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
    |> Fmt.fmt "hocc: Searching for remergeable state subgraphs"
    |> Io.with_log io
  in
  let remergeables = Remergeables.empty in
  let io, remergeables =
    (* Initialize the work list with indices of all states in non-singleton isocore sets. *)
    Isocores.fold_isocore_sets ~init:[] ~f:(fun state_indexes isocore_set ->
      match Ordset.length isocore_set with
      | 0L -> not_reached ()
      | 1L -> state_indexes
      | _ -> Ordset.fold ~init:state_indexes ~f:(fun workq index -> index :: workq) isocore_set
    ) isocores
    (* Reverse the work list so that remerging tends to follow the same order as splits occurred. *)
    |> List.rev
    |> List.fold ~init:(io, remergeables)
      ~f:(fun (io, remergeables) index0 ->
        let State.{statenub=statenub0; _} = Array.get index0 states in
        let StateNub.{isocore_set_sn=issn0; _} = statenub0 in
        let core = Lr1Itemset.core StateNub.(statenub0.lr1itemsetclosure).kernel in
        let isocore_set = Isocores.get_isocore_set_hlt core isocores in
        Ordset.fold_until ~init:(io, remergeables)
          ~f:(fun (io, remergeables) index1 ->
            let State.{statenub=statenub1; _} = Array.get index1 states in
            let StateNub.{isocore_set_sn=issn1; _} = statenub1 in
            (* Eliminate redundant/self pairs via `>`. Furthermore, use issn for comparison rather
             * than state index so that the order of merge attempts follows the same order as splits
             * occurred. For example, given a set {0,1,2,3,4}, the order of compatibility tests
             * during splitting was:
             *
             *   (1,0)
             *   (2,0), (2,1)
             *   (3,0), (3,1), (3,2)
             *   (4,0), (4,1), (4,2), (4,3) *)
            match issn0 > issn1 with
            | false -> (io, remergeables), true
            | true -> begin
                let io, remergeables = match Remergeables.rel statenub0 statenub1 remergeables with
                  | Distinct
                  | Mergeable -> io, remergeables
                  | Unknown -> begin
                      let remergeables, remergeable =
                        remergeable_statenubs states remergeables statenub0 statenub1 in
                      match remergeable with
                      | NotRemergeable spines -> begin
                          let io =
                            io.log
                            |> Fmt.fmt "."
                            |> Io.with_log io
                          in
                          io, Remergeables.distinct spines remergeables
                        end
                      | Remergeable -> begin
                          let io =
                            io.log
                            |> Fmt.fmt "+"
                            |> Uns.pp (Remergeables.subgraph_size remergeables)
                            |> Io.with_log io
                          in
                          io, Remergeables.mergeable remergeables
                        end
                    end
                in
                (io, remergeables), false
              end
          ) isocore_set
      )
  in
  let io =
    io.log
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  io, remergeables

let remerge_states io symbols isocores states =
  let io, remergeables = remergeable_search io isocores states in
  let remergeable_index_map = Remergeables.index_map remergeables in
  let nremergeable = Ordmap.length remergeable_index_map in
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp nremergeable |> Fmt.fmt " remergeable state"
    |> (fun formatter ->
      match nremergeable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  match nremergeable with
  | 0L -> io, isocores, states
  | _ -> begin
      let remaining_state_indexes = Range.Uns.fold (0L =:< Array.length states)
        ~init:(Ordset.empty (module State.Index))
        ~f:(fun reachable_state_indexes i ->
          match Ordmap.mem i remergeable_index_map with
          | true -> reachable_state_indexes
          | false -> Ordset.insert i reachable_state_indexes
        ) in
      let nremaining = Ordset.length remaining_state_indexes in
      let io =
        io.log
        |> Fmt.fmt "hocc: Reindexing " |> Uns.pp nremaining |> Fmt.fmt " LR(1) state"
        |> (fun formatter -> match nremaining with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
        |> Io.with_log io
      in
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
            let remerged_isocores =
              Isocores.remerge symbols remergeable_index_map index0 index1 remerged_isocores in
            remerged_isocores
          ) remergeable_index_map
        |> Isocores.reindex state_index_map in
      (* Remerge states. *)
      let remerged_states = Ordmap.fold ~init:states
          ~f:(fun remerged_states (index0, index1) ->
            assert State.Index.(index0 > index1);
            let state0 = Array.get index0 remerged_states in
            let state1 = Array.get index1 remerged_states in
            let state1' = State.remerge symbols remergeable_index_map state0 state1 in
            let remerged_states = Array.set index1 state1' remerged_states in
            remerged_states
          ) remergeable_index_map in
      (* Create a new set of reindexed states. *)
      let reindexed_states =
        Ordset.fold ~init:(Ordset.empty (module State)) ~f:(fun reindexed_states state_index ->
          let reindexed_state =
            Array.get state_index remerged_states
            |> State.reindex state_index_map None in
          Ordset.insert reindexed_state reindexed_states
        ) remaining_state_indexes
        |> Ordset.to_array in
      let io =
        io.log
        |> Fmt.fmt "\n"
        |> Io.with_log io
      in
      io, reindexed_isocores, reindexed_states
    end
