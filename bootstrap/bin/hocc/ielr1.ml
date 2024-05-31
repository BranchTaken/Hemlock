open Basis
open! Basis.Rudiments

let rec ipred_transit_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_transit_attribs
    lanectx =
  let state_index = State.index (LaneCtx.state lanectx) in
  (* Accumulate transit attribs and ipred lane contexts of `lanectx`. *)
  let lalr1_transit_attribs, lanectx =
    Array.fold ~init:(lalr1_transit_attribs, lanectx)
      ~f:(fun (lalr1_transit_attribs, lanectx) ipred_state_index ->
        let ipred_state = Array.get ipred_state_index lalr1_states in
        let ipred_lanectx = LaneCtx.of_ipred ipred_state lanectx in
        let ipred_kernel_attribs = LaneCtx.kernel_attribs_all ipred_lanectx in
        let transit = LaneCtx.transit ipred_lanectx in
        (* Load any existing transit attribs, whether to other conflict states, or as a result of
         * recursing into a lane cycle. *)
        let transit_attribs =
          Ordmap.get transit lalr1_transit_attribs
          |> Option.value ~default:TransitAttribs.empty
        in
        let kernel_attribs_all = TransitAttribs.kernel_attribs_all transit_attribs in
        (* Detect the no-op case as quickly as possible. The conceptually simpler approach of doing
         * the insertion and diffing before/after kernel attribs is a lot more expensive. *)
        let do_insert = KernelAttribs.for_any ~f:(fun (lr1item, attribs) ->
          match KernelAttribs.get lr1item kernel_attribs_all with
          | None -> true
          | Some attribs_prev -> begin
              Attribs.for_any ~f:(fun (Attrib.{conflict_state_index; symbol_index; _} as attrib) ->
                match Attribs.get ~conflict_state_index ~symbol_index attribs_prev with
                | None -> true
                | Some attrib_prev -> not Attrib.(is_empty (diff attrib attrib_prev))
              ) attribs
            end
        ) ipred_kernel_attribs in
        (* Avoid recursing if no new transit attribs are inserted, since no additional insertions
         * will occur in the recursion. *)
        let lalr1_transit_attribs = match do_insert with
          | false -> lalr1_transit_attribs
          | true -> begin
              let transit_attribs' =
                TransitAttribs.insert_kernel_attribs_all ipred_kernel_attribs transit_attribs in
              let lalr1_transit_attribs =
                Ordmap.upsert ~k:transit ~v:transit_attribs' lalr1_transit_attribs in
              (* Recurse if lanes may extend to predecessors. *)
              match LaneCtx.traces_length ipred_lanectx with
              | 0L -> lalr1_transit_attribs
              | _ -> ipred_transit_attribs ~resolve symbols prods lalr1_states adjs
                  ~lalr1_transit_attribs ipred_lanectx
            end
        in
        let lanectx = LaneCtx.incr_init ipred_lanectx lanectx in
        lalr1_transit_attribs, lanectx
      ) (Adjs.ipreds_of_state_index state_index adjs)
  in
  (* Finish computing definite attributions for `lanectx`. This is done post-order to detect
   * attributions for which there is a relevant kernel item in `lanectx`, but no relevant item in
   * any of its ipreds' lane contexts. *)
  let lanectx = LaneCtx.post_init lanectx in
  (* Accumulate definite attributions. *)
  let transit = LaneCtx.transit lanectx in
  let lane_attribs_definite = LaneCtx.lane_attribs_definite lanectx in
  match Attribs.is_empty lane_attribs_definite with
  | true -> lalr1_transit_attribs
  | false -> begin
      let transit_attribs_definite = TransitAttribs.of_attribs_definite lane_attribs_definite in
      Ordmap.amend transit ~f:(function
        | None -> Some transit_attribs_definite
        | Some transit_attribs_existing ->
          Some (TransitAttribs.union transit_attribs_definite transit_attribs_existing)
      ) lalr1_transit_attribs
    end

let gather_transit_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_transit_attribs
    conflict_state =
  let lanectx = LaneCtx.of_conflict_state ~resolve symbols prods conflict_state in
  ipred_transit_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_transit_attribs lanectx

let filter_transits_relevant lalr1_transit_attribs transits ~conflict_state_index symbol_index =
  (* Filter in/out transits lacking a relevant {conflict_state, symbol} attrib. *)
  Ordset.filter ~f:(fun in_transit ->
    Ordmap.get_hlt in_transit lalr1_transit_attribs
    |> TransitAttribs.all
    |> Attribs.get ~conflict_state_index ~symbol_index
    |> Option.is_some
  ) transits

let close_transit_attribs io adjs lalr1_transit_attribs =
  (* Backpropagate potential attribs, such that all lane predecessors make equivalent potential
   * attribs. *)
  let backward_propagate io lalr1_transit_attribs workq ~in_transits ~out_transits = begin
    let lane_attribs_potential = Ordset.fold ~init:Attribs.empty
        ~f:(fun lane_attribs_potential out_transit ->
          Ordmap.get out_transit lalr1_transit_attribs
          |> Option.value ~default:TransitAttribs.empty
          |> TransitAttribs.all
          |> Attribs.union lane_attribs_potential
        ) out_transits in
    let transit_attribs_potential = TransitAttribs.of_attribs_potential lane_attribs_potential in
    Ordset.fold ~init:(io, lalr1_transit_attribs, workq)
      ~f:(fun (io, lalr1_transit_attribs, workq)
        (Transit.{src=ipred_state_index; _} as in_transit) ->
        let in_transit_attribs =
          Ordmap.get in_transit lalr1_transit_attribs
          |> Option.value ~default:TransitAttribs.empty in
        let in_transit_attribs_all = TransitAttribs.all in_transit_attribs in
        (* Detect the no-op case as quickly as possible. The conceptually simpler approach of
         * performing the union and diffing before/after transit attribs is a lot more expensive. *)
        let do_union = Attribs.for_any
            ~f:(fun Attrib.{conflict_state_index; symbol_index; contrib=lane_contrib; _} ->
              match Attribs.get ~conflict_state_index ~symbol_index in_transit_attribs_all with
              | None -> true
              | Some Attrib.{contrib=transit_contrib; _} ->
                not Contrib.(is_empty (diff lane_contrib transit_contrib))
            ) lane_attribs_potential in
        match do_union with
        | false -> io, lalr1_transit_attribs, workq
        | true -> begin
            let in_transit_attribs' =
              TransitAttribs.union transit_attribs_potential in_transit_attribs in
            let lalr1_transit_attribs = Ordmap.upsert ~k:in_transit ~v:in_transit_attribs'
                lalr1_transit_attribs in
            let io, workq = match Workq.mem ipred_state_index workq with
              | true -> io, workq
              | false -> begin
                  let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
                  io, Workq.push_back ipred_state_index workq
                end
            in
            io, lalr1_transit_attribs, workq
          end
      ) in_transits
  end in
  let rec work_backward io adjs lalr1_transit_attribs workq = begin
    match Workq.is_empty workq with
    | true -> io, lalr1_transit_attribs
    | false -> begin
        let io = io.log |> Fmt.fmt "." |> Io.with_log io in
        let state_index, workq = Workq.pop workq in
        let in_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun in_transits ipred_state_index ->
            let in_transit = Transit.init ~src:ipred_state_index ~dst:state_index in
            Ordset.insert in_transit in_transits
          ) (Adjs.ipreds_of_state_index state_index adjs) in
        (* Filter out transits for which there are no conflict attributions, since they contain
         * nothing to backpropagate. Also look for a self-transit, which is specific to conflict
         * states. *)
        let out_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun out_transits isucc_state_index ->
            let out_transit = Transit.init ~src:state_index ~dst:isucc_state_index in
            match Ordmap.get out_transit lalr1_transit_attribs with
            | None -> out_transits
            | Some _ -> Ordset.insert out_transit out_transits
          ) (Array.append state_index (Adjs.isuccs_of_state_index state_index adjs)) in
        let io, lalr1_transit_attribs, workq = backward_propagate io lalr1_transit_attribs workq
            ~in_transits ~out_transits in
        work_backward io adjs lalr1_transit_attribs workq
      end
  end in
  (* Propagate definite attribs forward wherever possible, until no further propagation is possible.
   * An attrib can be propagated forward if all in-transitions with relevant attributions make the
   * same definite contribution.
   *
   * Note that propagation is on a per attribution basis, and for each propagation attempt, only
   * in/out-transitions with the relevant {conflict state, symbol} are considered and propagated
   * from/to. *)
  let forward_propagate io lalr1_transit_attribs workq ~in_transits ~out_transits = begin
    match Ordset.is_empty in_transits with
    | true -> begin
        (* Start state. Propagate shift attribs forward. *)
        let io, lalr1_transit_attribs, workq =
          Ordset.fold ~init:(io, lalr1_transit_attribs, workq)
            ~f:(fun (io, lalr1_transit_attribs, workq) out_transit ->
              let out_transit_attribs = Ordmap.get_hlt out_transit lalr1_transit_attribs in
              let out_attribs_all = out_transit_attribs |> TransitAttribs.all in
              let out_transit_attribs' = Attribs.fold ~init:out_transit_attribs
                  ~f:(fun out_transit_attribs
                    Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
                    match Contrib.mem_shift contrib with
                    | false -> out_transit_attribs
                    | true -> begin
                        let shift_attrib = Attrib.init_lane ~conflict_state_index ~symbol_index
                            ~conflict ~contrib:Contrib.shift in
                        TransitAttribs.merge_definite shift_attrib out_transit_attribs
                      end
                  ) out_attribs_all in
              match Attribs.equal (TransitAttribs.definite out_transit_attribs')
                (TransitAttribs.definite out_transit_attribs) with
              | true -> io, lalr1_transit_attribs, workq
              | false -> begin
                  let lalr1_transit_attribs = Ordmap.update_hlt ~k:out_transit
                      ~v:out_transit_attribs' lalr1_transit_attribs in
                  let isucc_state_index = Transit.(out_transit.dst) in
                  let io, workq = match Workq.mem isucc_state_index workq with
                    | true -> io, workq
                    | false -> begin
                        let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
                        io, Workq.push_back isucc_state_index workq
                      end
                  in
                  io, lalr1_transit_attribs, workq
                end
            ) out_transits in
        io, lalr1_transit_attribs, workq
      end
    | false -> begin
        let in_attribs_definite = Ordset.fold ~init:Attribs.empty
            ~f:(fun in_attribs_definite transit ->
              let lane_attribs_definite =
                Ordmap.get_hlt transit lalr1_transit_attribs
                |> TransitAttribs.definite in
              Attribs.union lane_attribs_definite in_attribs_definite
            ) in_transits in
        let io, lalr1_transit_attribs, workq =
          Attribs.fold ~init:(io, lalr1_transit_attribs, workq)
            ~f:(fun (io, lalr1_transit_attribs, workq)
              Attrib.{conflict_state_index; symbol_index; conflict;
              contrib=in_contrib_definite; _} ->
              let in_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  in_transits ~conflict_state_index symbol_index in
              let out_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  out_transits ~conflict_state_index symbol_index in
              let io, lalr1_transit_attribs, workq =
                (* Determine whether there exists a common definite in-contrib, the existence
                 * of which allows propagation. *)
                let in_contrib_common =
                  Ordset.fold_until ~init:in_contrib_definite
                    ~f:(fun in_contrib_common in_transit ->
                      let attrib_opt =
                        Ordmap.get_hlt in_transit lalr1_transit_attribs
                        |> TransitAttribs.definite
                        |> Attribs.get ~conflict_state_index ~symbol_index in
                      let contrib = match attrib_opt with
                        | Some Attrib.{contrib; _} -> contrib
                        | None -> Contrib.empty in
                      let in_contrib_common = Contrib.inter contrib in_contrib_common in
                      in_contrib_common, Contrib.is_empty in_contrib_common
                    ) in_transits_relevant in
                match Contrib.is_empty in_contrib_common with
                | true -> io, lalr1_transit_attribs, workq
                | false -> begin
                    (* Propagate forward. *)
                    Ordset.fold ~init:(io, lalr1_transit_attribs, workq)
                      ~f:(fun (io, lalr1_transit_attribs, workq) out_transit ->
                        let out_transit_attribs =
                          Ordmap.get_hlt out_transit lalr1_transit_attribs in
                        let lane_attrib = Attrib.init_lane ~conflict_state_index ~symbol_index
                            ~conflict ~contrib:in_contrib_common in
                        let out_transit_attribs' =
                          TransitAttribs.merge_definite lane_attrib out_transit_attribs in
                        match Attribs.equal (TransitAttribs.definite out_transit_attribs')
                          (TransitAttribs.definite out_transit_attribs) with
                        | true -> io, lalr1_transit_attribs, workq
                        | false -> begin
                            let lalr1_transit_attribs = Ordmap.update_hlt ~k:out_transit
                                ~v:out_transit_attribs' lalr1_transit_attribs in
                            let isucc_state_index = Transit.(out_transit.dst) in
                            let io, workq = match Workq.mem isucc_state_index workq with
                              | true -> io, workq
                              | false -> begin
                                  let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
                                  io, Workq.push_back isucc_state_index workq
                                end
                            in
                            io, lalr1_transit_attribs, workq
                          end
                      ) out_transits_relevant
                  end
              in
              io, lalr1_transit_attribs, workq
            ) in_attribs_definite in
        io, lalr1_transit_attribs, workq
      end
  end in
  let rec work_forward io adjs lalr1_transit_attribs workq = begin
    match Workq.is_empty workq with
    | true -> io, lalr1_transit_attribs
    | false -> begin
        let io = io.log |> Fmt.fmt "." |> Io.with_log io in
        let state_index, workq = Workq.pop workq in
        (* Filter in/out transits for which there are no conflict attributions, since they lie
         * outside any relevant lane. *)
        let in_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun in_transits ipred_state_index ->
            let in_transit = Transit.init ~src:ipred_state_index ~dst:state_index in
            match Ordmap.get in_transit lalr1_transit_attribs with
            | None -> in_transits
            | Some _ -> Ordset.insert in_transit in_transits
          ) (Adjs.ipreds_of_state_index state_index adjs) in
        let out_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun out_transits isucc_state_index ->
            let out_transit = Transit.init ~src:state_index ~dst:isucc_state_index in
            match Ordmap.get out_transit lalr1_transit_attribs with
            | None -> out_transits
            | Some _ -> Ordset.insert out_transit out_transits
          ) (Adjs.isuccs_of_state_index state_index adjs) in
        let io, lalr1_transit_attribs, workq = forward_propagate io lalr1_transit_attribs workq
            ~in_transits ~out_transits in
        work_forward io adjs lalr1_transit_attribs workq
      end
  end in
  let init_workq lalr1_transit_attribs = begin
    Ordmap.fold ~init:Workq.empty
      ~f:(fun workq (Transit.{src; dst}, _transit_attribs) ->
        let merge_state_index state_index workq = begin
          match Workq.mem state_index workq with
          | true -> workq
          | false -> Workq.push_back state_index workq
        end in
        workq
        |> merge_state_index src
        |> merge_state_index dst
      ) lalr1_transit_attribs
  end in
  (* Gather the set of states in conflict-contributing lanes that have (definite) attributions on
   * adjacent transits. Once backpropagation completes, all conflict-contributing lanes will have
   * attributions on all constituent transits. This means the initial work queue for `work_forward`
   * may be larger than that for `work_backward`. *)
  let io, lalr1_transit_attribs =
    work_backward io adjs lalr1_transit_attribs (init_workq lalr1_transit_attribs) in
  work_forward io adjs lalr1_transit_attribs (init_workq lalr1_transit_attribs)

let close_stable ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs =
  let work_finish io ~stable workq = begin
    (* Remaining queued states are split-stable, because all transitively reachable states in the
     * `pred_stability_deps` graph are also queued. *)
    let reqd = Workq.set workq in
    let io =
      io.log
      |> String.fmt ~pad:(Codepoint.of_char '.') ~width:(Set.length reqd) ""
      |> Io.with_log io
    in
    let stable = Set.union reqd stable in
    io, stable
  end in
  let gather_transits state_index lalr1_transit_attribs adjs = begin
    (* Filter in/out transits for which there are no conflict attributions, since they lie outside
     * any relevant lane. *)
    let in_transits_all = Array.fold ~init:(Ordset.empty (module Transit))
      ~f:(fun in_transits_all ipred_state_index ->
        let transit = Transit.init ~src:ipred_state_index ~dst:state_index in
        match Ordmap.get transit lalr1_transit_attribs with
        | None -> in_transits_all
        | Some _ -> Ordset.insert transit in_transits_all
      ) (Adjs.ipreds_of_state_index state_index adjs) in
    let out_transits_all = Array.fold ~init:(Ordset.empty (module Transit))
      ~f:(fun out_transits_all isucc_state_index ->
        let transit = Transit.init ~src:state_index ~dst:isucc_state_index in
        match Ordmap.get transit lalr1_transit_attribs with
        | None -> out_transits_all
        | Some _ -> Ordset.insert transit out_transits_all
      ) (Adjs.isuccs_of_state_index state_index adjs) in
    (in_transits_all, out_transits_all)
  end in
  let gather_in_attribs lalr1_transit_attribs in_transits_all = begin
    (* Gather the set of all in-attribs, which is a non-strict subset of all out-attribs
     * (out-transitions may make definite attributions). *)
    Ordset.fold ~init:Attribs.empty
      ~f:(fun in_attribs_all transit ->
        let lane_attribs =
          Ordmap.get_hlt transit lalr1_transit_attribs
          |> TransitAttribs.all in
        Attribs.union lane_attribs in_attribs_all
      ) in_transits_all
  end in
  let is_split_unstable_self ~resolve symbols prods ~lalr1_transit_attribs
      (Attrib.{conflict_state_index; symbol_index; _} as manifestation_attrib)
      in_transits_relevant = begin
    (* Self-contributing conflict state. For all relevant in-transitions considered in turn as if
     * the state were split from all other in-transitions, the state is split-stable if the
     * resolution of the in-contribution is the same as the non-split case. *)
    let split_unstable = Ordset.for_any ~f:(fun in_transit ->
      let in_transit_attrib =
        Ordmap.get_hlt in_transit lalr1_transit_attribs
        |> TransitAttribs.all
        |> Attribs.get_hlt ~conflict_state_index ~symbol_index
      in
      not (Attrib.compat_ielr1 ~resolve symbols prods in_transit_attrib manifestation_attrib)
    ) in_transits_relevant in
    split_unstable
  end in
  let is_split_unstable ~resolve symbols prods ~lalr1_transit_attribs state_index
      (Attrib.{conflict_state_index; conflict; symbol_index; _} as attrib) in_transits_relevant
      out_transits_relevant = begin
    match State.Index.(conflict_state_index = state_index) with
    | true ->
      is_split_unstable_self ~resolve symbols prods ~lalr1_transit_attribs attrib
        in_transits_relevant
    | false -> begin
        (* Consider each relevant (in,out)-transition pair as if the state were split from all other
         * in-transitions. Note that if there's only one in-transition, the state cannot be split,
         * and is trivially split-stable. *)
        let empty_attrib = Attrib.empty ~conflict_state_index ~symbol_index ~conflict in
        let split_unstable =
          Uns.( > ) (Ordset.length in_transits_relevant) 1L &&
          Ordset.for_any ~f:(fun in_transit ->
            let in_attrib_all =
              Ordmap.get_hlt in_transit lalr1_transit_attribs
              |> TransitAttribs.all
              |> Attribs.get_hlt ~conflict_state_index ~symbol_index
            in
            let in_attrib_definite =
              Ordmap.get in_transit lalr1_transit_attribs
              |> Option.value ~default:TransitAttribs.empty
              |> TransitAttribs.definite
              |> Attribs.get ~conflict_state_index ~symbol_index
              |> Option.value ~default:empty_attrib
            in
            Ordset.for_any ~f:(fun out_transit ->
              let out_attrib_all =
                Ordmap.get_hlt out_transit lalr1_transit_attribs
                |> TransitAttribs.all
                |> Attribs.get_hlt ~conflict_state_index ~symbol_index
              in
              let out_attrib_definite =
                Ordmap.get out_transit lalr1_transit_attribs
                |> Option.value ~default:TransitAttribs.empty
                |> TransitAttribs.definite
                |> Attribs.get ~conflict_state_index ~symbol_index
                |> Option.value ~default:empty_attrib
              in
              (* The state is split-unstable if the dominant out-contribution is unstable, or if the
               * dominant out-contribution changes between the split/unsplit cases. *)
              let unsplit_out_attrib_all = out_attrib_all in
              let unsplit_out_attrib_definite = out_attrib_definite in
              let split_out_attrib_all = Attrib.union in_attrib_all out_attrib_definite in
              let split_out_attrib_definite = Attrib.union in_attrib_definite out_attrib_definite in
              let is_unsplit_dominant_contrib_unstable = not (Attrib.equal_ielr1 ~resolve symbols prods
                  unsplit_out_attrib_all unsplit_out_attrib_definite) in
              let is_split_dominant_contrib_unstable = not (Attrib.equal_ielr1 ~resolve symbols prods
                  split_out_attrib_all split_out_attrib_definite) in
              let does_dominant_contrib_change = not (Attrib.equal_ielr1 ~resolve symbols prods
                  unsplit_out_attrib_definite split_out_attrib_definite) in
              is_unsplit_dominant_contrib_unstable ||
              is_split_dominant_contrib_unstable ||
              does_dominant_contrib_change
            ) out_transits_relevant
          ) in_transits_relevant in
        split_unstable
      end
  end in
  let gather_pred_stability_deps_indexes ~resolve symbols prods ~lalr1_transit_attribs ~stable
      ~unstable is_pred_stability_deps_stale ipred_split_unstable pred_stability_deps_indexes
      Attrib.{conflict_state_index; conflict; symbol_index; _} in_transits_relevant = begin
    let ipred_split_unstable, pred_stability_deps_indexes =
      match ipred_split_unstable with
      | true -> ipred_split_unstable, pred_stability_deps_indexes
      | false -> begin
          Ordset.fold_until
            ~init:(ipred_split_unstable, pred_stability_deps_indexes)
            ~f:(fun (ipred_split_unstable, pred_stability_deps_indexes)
              (Transit.{src=ipred_state_index; _} as in_transit) ->
              let is_ipred_stable = Set.mem ipred_state_index stable in
              let ipred_split_unstable, pred_stability_deps_indexes =
                match is_ipred_stable with
                | true -> ipred_split_unstable, pred_stability_deps_indexes
                | false -> begin
                    let is_ipred_dominant_contrib_unstable =
                      match is_pred_stability_deps_stale with
                      | false -> begin
                          (* Use previously computed dependency info to avoid recomputing dominant
                           * contribution instability. *)
                          let is_ipred_dominant_contrib_unstable =
                            Set.mem ipred_state_index pred_stability_deps_indexes in
                          is_ipred_dominant_contrib_unstable
                        end
                      | true -> begin
                          let transit_attribs = Ordmap.get_hlt in_transit lalr1_transit_attribs in
                          let attrib_all =
                            transit_attribs
                            |> TransitAttribs.all
                            |> Attribs.get_hlt ~conflict_state_index ~symbol_index in
                          let empty_attrib =
                            Attrib.empty ~conflict_state_index ~symbol_index ~conflict in
                          let attrib_definite =
                            transit_attribs
                            |> TransitAttribs.definite
                            |> Attribs.get ~conflict_state_index ~symbol_index
                            |> Option.value ~default:empty_attrib
                          in
                          not (Attrib.equal_ielr1 ~resolve symbols prods attrib_all
                              attrib_definite)
                        end
                    in
                    let is_ipred_unstable = Set.mem ipred_state_index unstable in
                    match is_ipred_dominant_contrib_unstable, is_ipred_unstable with
                    | false, _ -> ipred_split_unstable, pred_stability_deps_indexes
                    | true, false -> begin
                        (* Split-stability depends on the ipred being split-stable, and the ipred's
                         * split-stability is currently undetermined. Record the dependency on the
                         * ipred and its transitive dependencies, and requeue. *)
                        ipred_split_unstable,
                        pred_stability_deps_indexes
                        |> Set.insert ipred_state_index
                      end
                    | true, true -> begin
                        (* Split-stability depends on the ipred being split-stable, and the ipred is
                         * already known to be split-unstable. *)
                        true, pred_stability_deps_indexes
                      end
                  end
              in
              (ipred_split_unstable, pred_stability_deps_indexes), ipred_split_unstable
            ) in_transits_relevant
        end
    in
    (ipred_split_unstable, pred_stability_deps_indexes)
  end in
  let rec work ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs
      ~stable ~pred_stability_deps ~isucc_stability_deps ~unstable churn workq = begin
    (* Terminate work if all workq items have been considered since the last forward progress. This
     * conveniently also terminates if the workq empties. *)
    let workq_length = Workq.length workq in
    match Uns.(churn < workq_length) with
    | false -> work_finish io ~stable workq
    | true -> begin
        assert (not (Workq.is_empty workq));
        let state_index, workq = Workq.pop workq in
        (* Check whether any of the state's pred stability dependencies have settled since the
         * previous time this state was considered (if at all). If any settling occurred, the
         * state's pred stability dependencies in `pred_stability_deps` are stale and dominant
         * contribution stability must be recomputed. *)
        let is_pred_stability_deps_stale, pred_stability_deps_indexes =
          match Ordmap.get state_index pred_stability_deps with
          | Some pred_stability_deps_indexes -> begin
              let is_pred_stability_deps_stale =
                (not (Set.disjoint pred_stability_deps_indexes stable)) ||
                (not (Set.disjoint pred_stability_deps_indexes unstable)) in
              is_pred_stability_deps_stale, pred_stability_deps_indexes
            end
          | None -> true, Set.empty (module State.Index)
        in
        let in_transits_all, out_transits_all =
          gather_transits state_index lalr1_transit_attribs adjs in
        let in_attribs_all = gather_in_attribs lalr1_transit_attribs in_transits_all in
        (* Test whether this state is split-stable with respect to each attribution. There are three
         * possible outcomes:
         * - The state is split-stable.
         * - The state is split-unstable.
         * - The state may be split-stable, if all its ipreds/isuccs are determined to be
         *   split-stable (i.e. ipred/isucc-dependent split-stability). *)
        let split_unstable, ipred_split_unstable, pred_stability_deps_indexes =
          Attribs.fold ~init:(false, false, pred_stability_deps_indexes)
            ~f:(fun (split_unstable, ipred_split_unstable, pred_stability_deps_indexes)
              (Attrib.{conflict_state_index; symbol_index; _} as attrib) ->
              let in_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  in_transits_all ~conflict_state_index symbol_index in
              let out_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  out_transits_all ~conflict_state_index symbol_index in
              let split_unstable = match split_unstable with
                | true -> true
                | false -> begin
                    is_split_unstable ~resolve symbols prods ~lalr1_transit_attribs state_index
                      attrib in_transits_relevant out_transits_relevant
                  end
              in
              let ipred_split_unstable, pred_stability_deps_indexes =
                gather_pred_stability_deps_indexes ~resolve symbols prods ~lalr1_transit_attribs
                  ~stable ~unstable is_pred_stability_deps_stale ipred_split_unstable
                  pred_stability_deps_indexes attrib in_transits_relevant
              in
              (split_unstable, ipred_split_unstable, pred_stability_deps_indexes)
            ) in_attribs_all
        in
        let open Tristate in
        let ipred_split_unstable_tri =
          match ipred_split_unstable, Set.is_empty pred_stability_deps_indexes with
          | false, true -> No
          | false, false -> Maybe
          | true, _ -> Yes
        in
        let io, stable, pred_stability_deps, isucc_stability_deps, unstable, churn, workq =
          match split_unstable, ipred_split_unstable_tri with
          | false, No -> begin
              (* Split-stable. *)
              io.log |> Fmt.fmt "." |> Io.with_log io,
              Set.insert state_index stable,
              Ordmap.remove state_index pred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              unstable,
              0L,
              workq
            end
          | true, _
          | _, Yes -> begin
              (* Split-unstable, or breaking cycle. *)
              io.log |> Fmt.fmt "^" |> Io.with_log io,
              stable,
              Ordmap.remove state_index pred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              Set.insert state_index unstable,
              0L,
              workq
            end
          | _, Maybe -> begin
              (* Possible ipred-dependent split-stability. *)
              io,
              stable,
              Ordmap.upsert ~k:state_index ~v:pred_stability_deps_indexes pred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              unstable,
              succ churn,
              Workq.push_back state_index workq
            end
        in
        work ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs
          ~stable ~pred_stability_deps ~isucc_stability_deps ~unstable churn workq
      end
  end in
  (* Gather the set of states in conflict-contributing lanes, excluding start states, which are
   * always split-stable. Start states are always transition sources (never destinations), which
   * makes them trivial to exclude. *)
  let workq = Ordmap.fold ~init:Workq.empty
      ~f:(fun workq (Transit.{dst; _}, _attribs) ->
        match Workq.mem dst workq with
        | true -> workq
        | false -> Workq.push_back dst workq
      ) lalr1_transit_attribs in
  (* Initialize the set of split-stable states as the complement of `workq`. *)
  let stable = Range.Uns.fold (0L =:< Isocores.length lalr1_isocores)
    ~init:(Set.empty (module StateNub.Index))
    ~f:(fun stable state_index ->
      match Workq.mem state_index workq with
      | true -> stable
      | false -> Set.insert state_index stable
    ) in
  (* Process the workq. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Closing IELR(1) state split-stability (^.=unstable/stable)"
    |> Io.with_log io
  in
  let io, stable = work ~resolve io symbols prods lalr1_isocores lalr1_states adjs
      ~lalr1_transit_attribs
      ~stable
      ~pred_stability_deps:(Ordmap.empty (module State.Index))
      ~isucc_stability_deps:(Ordmap.empty (module State.Index))
      ~unstable:(Set.empty (module State.Index))
      0L
      workq in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  let unstable_states = (Isocores.length lalr1_isocores) - (Set.length stable) in
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp unstable_states
    |> Fmt.fmt " split-unstable state"
    |> (fun formatter -> match unstable_states with
      | 1L -> formatter
      | _ -> formatter |> Fmt.fmt "s"
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  io, stable

let lalr1_transit_attribs_init ~resolve io symbols prods lalr1_isocores lalr1_states =
  let adjs = Adjs.init lalr1_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let io, lalr1_transit_attribs =
    Array.fold ~init:(io, Ordmap.empty (module Transit))
      ~f:(fun (io, lalr1_transit_attribs) state ->
        match State.has_conflict_attribs ~resolve symbols prods state with
        | false -> io, lalr1_transit_attribs
        | true -> begin
            let io = io.log |> Fmt.fmt "." |> Io.with_log io in
            let lalr1_transit_attribs =
              gather_transit_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_transit_attribs
                state in
            io, lalr1_transit_attribs
          end
      ) lalr1_states
  in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  let io =
    io.log
    |> Fmt.fmt "hocc: Closing IELR(1) conflict attributions (+.=propagate/quiesce)"
    |> Io.with_log io
  in
  let io, lalr1_transit_attribs = close_transit_attribs io adjs lalr1_transit_attribs in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  (* Determine state split-stability. *)
  let io, lalr1_isocores_stable = close_stable ~resolve io symbols prods lalr1_isocores lalr1_states
      adjs ~lalr1_transit_attribs in
  (* Filter out transit attribs to split-stable states. *)
  let lalr1_transit_attribs = Ordmap.filter ~f:(fun (Transit.{dst; _}, _transit_attribs) ->
    not (Set.mem dst lalr1_isocores_stable)
  ) lalr1_transit_attribs in
  io, lalr1_transit_attribs

(* Create lookup function for attribs that closes on the prerequisite LALR(1) inadequacy analysis.
*)
let gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states =
  let io, lalr1_transit_attribs =
    lalr1_transit_attribs_init ~resolve io symbols prods lalr1_isocores lalr1_states in
  let transit_of_statenub_goto statenub goto = begin
    let statenub_core = (Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure.kernel)) in
    let goto_core = Lr1Itemset.core goto in
    let src = Isocores.get_core_hlt statenub_core lalr1_isocores in
    let dst = Isocores.get_core_hlt goto_core lalr1_isocores in
    Transit.init ~src ~dst
  end in
  let isocores_sn_of_transit Transit.{dst; _} =
    Isocores.statenub dst lalr1_isocores
    |> StateNub.isocores_sn
  in
  let gotonub_of_statenub_goto statenub goto = begin
    let transit = transit_of_statenub_goto statenub goto in
    let isocores_sn = isocores_sn_of_transit transit in
    let transit_attribs = match Ordmap.get transit lalr1_transit_attribs with
      | None -> TransitAttribs.empty
      | Some transit_attribs -> transit_attribs
    in
    GotoNub.init ~isocores_sn_opt:(Some isocores_sn) ~goto ~transit_attribs
  end in
  io, gotonub_of_statenub_goto
