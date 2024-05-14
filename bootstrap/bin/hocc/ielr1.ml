open Basis
open! Basis.Rudiments

(* Backpropagate attribs that were directly attributed, such that all lane predecessors make
 * equivalent indirect attribs. *)
let rec backprop_transit_attribs adjs transit_attribs lalr1_transit_attribs marks state_index =
  Array.fold ~init:lalr1_transit_attribs
    ~f:(fun lalr1_transit_attribs ipred_state_index ->
      match Set.mem ipred_state_index marks with
      | true -> lalr1_transit_attribs
      | false -> begin
          let transit = Transit.init ~src:ipred_state_index ~dst:state_index in
          assert (not (Transit.cyclic transit));
          let transit_attribs_prev = match Ordmap.get transit lalr1_transit_attribs with
            | None -> TransitAttribs.empty
            | Some transit_attribs_prev -> transit_attribs_prev
          in
          let transit_attribs_union = TransitAttribs.union transit_attribs transit_attribs_prev in
          match Attribs.equal (TransitAttribs.all transit_attribs_union)
            (TransitAttribs.all transit_attribs_prev) with
          | true -> lalr1_transit_attribs
          | false -> begin
              let lalr1_transit_attribs = Ordmap.upsert ~k:transit ~v:transit_attribs_union
                  lalr1_transit_attribs in
              let marks = Set.insert ipred_state_index marks in
              backprop_transit_attribs adjs transit_attribs lalr1_transit_attribs marks
                ipred_state_index
            end
        end
    ) (Adjs.ipreds_of_state_index state_index adjs)

let rec ipred_transit_attribs ~resolve lalr1_states adjs ~lalr1_transit_attribs marks lanectx =
  (* Marking of the current lane segment spanning the start state back to the current state prevents
   * infinite recursion. It is possible for a grammar to induce a combinatorial explosion of
   * contributing lanes, but only non-redundant transition attribs lead to recursion, thus assuring
   * that each transition is recursed on only once. *)
  let state_index = State.index (LaneCtx.state lanectx) in
  assert (not (Set.mem state_index marks));
  let marks = Set.insert state_index marks in
  (* Accumulate transit attribs and ipred lane contexts of `lanectx`. *)
  let lalr1_transit_attribs, ipred_lanectxs =
    Array.fold ~init:(lalr1_transit_attribs, [])
      ~f:(fun (lalr1_transit_attribs, ipred_lanectxs) ipred_state_index ->
        let ipred_state = Array.get ipred_state_index lalr1_states in
        let ipred_lanectx = LaneCtx.of_ipred ipred_state lanectx in
        let ipred_kernel_attribs = LaneCtx.kernel_attribs ipred_lanectx in
        let transit = LaneCtx.transit ipred_lanectx in
        (* Load current transit attribs. It is possible for there to be existing attribs to other
         * conflict states. *)
        let transit_attribs =
          Ordmap.get transit lalr1_transit_attribs
          |> Option.value ~default:TransitAttribs.empty
        in
        let kernel_attribs = TransitAttribs.kernel_attribs transit_attribs in
        let transit_attribs' =
          TransitAttribs.insert_kernel_attribs ipred_kernel_attribs transit_attribs in
        (* Avoid recursing if no new transit attribs were inserted, since no additional insertions
         * will occur in the recursion. *)
        let kernel_attribs' = TransitAttribs.kernel_attribs transit_attribs' in
        let lalr1_transit_attribs =
          match KernelAttribs.equal kernel_attribs' kernel_attribs with
          | true -> lalr1_transit_attribs
          | false -> begin
              assert (not (Transit.cyclic transit));
              let lalr1_transit_attribs =
                Ordmap.upsert ~k:transit ~v:transit_attribs' lalr1_transit_attribs in
              (* Recurse if lanes may extend to predecessors. *)
              match LaneCtx.traces_length ipred_lanectx with
              | 0L -> lalr1_transit_attribs
              | _ -> ipred_transit_attribs ~resolve lalr1_states adjs ~lalr1_transit_attribs
                  marks ipred_lanectx
            end
        in
        let ipred_lanectxs = ipred_lanectx :: ipred_lanectxs in
        lalr1_transit_attribs, ipred_lanectxs
      ) (Array.filter ~f:(fun ipred_state_index -> not (Set.mem ipred_state_index marks))
        (Adjs.ipreds_of_state_index state_index adjs))
  in
  (* Finish computing direct attributions for `lanectx`. This is done post-order to detect
   * attributions for which there is a relevant kernel item in `lanectx`, but no relevant item in
   * any of its ipreds' lane contexts. *)
  let lanectx = LaneCtx.post_init ipred_lanectxs lanectx in
  (* Accumulate direct attributions. *)
  let transit = LaneCtx.transit lanectx in
  let lane_attribs_direct = LaneCtx.lane_attribs_direct lanectx in
  let lalr1_transit_attribs = match Attribs.is_empty lane_attribs_direct with
    | true -> lalr1_transit_attribs
    | false -> begin
        (* Backpropagate. *)
        let transit_attribs = TransitAttribs.of_lane_attribs lane_attribs_direct in
        let lalr1_transit_attribs = backprop_transit_attribs adjs transit_attribs
            lalr1_transit_attribs marks state_index in
        let lalr1_transit_attribs = match Transit.cyclic transit with
          | true -> lalr1_transit_attribs
          | false -> begin
              let transit_attribs_direct =
                TransitAttribs.of_lane_attribs_direct lane_attribs_direct in
              Ordmap.amend transit ~f:(function
                | None -> Some transit_attribs_direct
                | Some transit_attribs_existing ->
                  Some (TransitAttribs.union transit_attribs_direct transit_attribs_existing)
              ) lalr1_transit_attribs
            end
        in
        lalr1_transit_attribs
      end
  in
  lalr1_transit_attribs

let gather_transit_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_transit_attribs
    conflict_state_index =
  let marks = Set.empty (module State.Index) in
  let conflict_state = Array.get conflict_state_index lalr1_states in
  let lanectx = LaneCtx.of_conflict_state ~resolve symbols prods conflict_state in
  ipred_transit_attribs ~resolve lalr1_states adjs ~lalr1_transit_attribs marks lanectx

let filter_transits_relevant lalr1_transit_attribs transits ~conflict_state_index symbol_index =
  (* Filter in/out transits lacking the relevant {conflict_state, symbol}. *)
  Ordset.filter ~f:(fun in_transit ->
    Ordmap.get_hlt in_transit lalr1_transit_attribs
    |> TransitAttribs.all
    |> Attribs.get ~conflict_state_index symbol_index
    |> Option.is_some
  ) transits

let close_transit_attribs io adjs lalr1_transit_attribs =
  (* Propagate attribs forward wherever possible, until no further propagation is possible. An
   * attrib can be propagated forward if all in-transitions with relevant attributions make the
   * same contribution.
   *
   * Note that propagation is on a per attribution basis, and for each propagation attempt, only
   * in/out-transitions with the relevant {conflict state, symbol} are considered and propagated
   * from/to. *)
  let rec work io adjs lalr1_transit_attribs workq = begin
    match Workq.is_empty workq with
    | true -> io, lalr1_transit_attribs
    | false -> begin
        let io = io.log |> Fmt.fmt "." |> Io.with_log io in
        let state_index, workq = Workq.pop workq in
        (* Filter in/out transits for which there are no conflict attributions, since they lie
         * outside any relevant lane. *)
        let in_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun in_transits ipred_state_index ->
            let transit = Transit.init ~src:ipred_state_index ~dst:state_index in
            match Ordmap.get transit lalr1_transit_attribs with
            | None -> in_transits
            | Some _ -> Ordset.insert transit in_transits
          ) (Adjs.ipreds_of_state_index state_index adjs) in
        let out_transits = Array.fold ~init:(Ordset.empty (module Transit))
          ~f:(fun out_transits isucc_state_index ->
            let transit = Transit.init ~src:state_index ~dst:isucc_state_index in
            match Ordmap.get transit lalr1_transit_attribs with
            | None -> out_transits
            | Some _ -> Ordset.insert transit out_transits
          ) (Adjs.isuccs_of_state_index state_index adjs) in
        let in_attribs_all = Ordset.fold ~init:Attribs.empty
            ~f:(fun attribs_all transit ->
              let lane_attribs =
                Ordmap.get_hlt transit lalr1_transit_attribs
                |> TransitAttribs.all in
              Attribs.union lane_attribs attribs_all
            ) in_transits in
        let io, lalr1_transit_attribs, workq =
          Attribs.fold ~init:(io, lalr1_transit_attribs, workq)
            ~f:(fun (io, lalr1_transit_attribs, workq)
              Attrib.{conflict_state_index; symbol_index; conflict; contrib=in_contrib_all; _} ->
              let in_transits_relevant = filter_transits_relevant lalr1_transit_attribs in_transits
                  ~conflict_state_index symbol_index in
              let out_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  out_transits ~conflict_state_index symbol_index in
              (* Determine whether there exists a common in-contrib, the existence of which allows
               * propagation. *)
              let in_contrib_common = Ordset.fold_until ~init:in_contrib_all
                  ~f:(fun in_contrib_common in_transit ->
                    let Attrib.{contrib; _} =
                      Ordmap.get_hlt in_transit lalr1_transit_attribs
                      |> TransitAttribs.all
                      |> Attribs.get_hlt ~conflict_state_index symbol_index in
                    let in_contrib_common = Contrib.inter contrib in_contrib_common in
                    in_contrib_common, Contrib.is_empty in_contrib_common
                  ) in_transits_relevant in
              let io, lalr1_transit_attribs, workq =
                match Contrib.is_empty in_contrib_common with
                | true -> io, lalr1_transit_attribs, workq
                | false -> begin
                    (* Propagate forward. *)
                    Ordset.fold ~init:(io, lalr1_transit_attribs, workq)
                      ~f:(fun (io, lalr1_transit_attribs, workq) out_transit ->
                        let transit_attribs =
                          Ordmap.get_hlt out_transit lalr1_transit_attribs in
                        let transit_attribs' = TransitAttribs.merge ~conflict_state_index
                            ~symbol_index ~conflict ~contrib:in_contrib_common transit_attribs in
                        match Attribs.equal (TransitAttribs.all transit_attribs')
                          (TransitAttribs.all transit_attribs) with
                        | true -> io, lalr1_transit_attribs, workq
                        | false -> begin
                            let lalr1_transit_attribs = Ordmap.update_hlt ~k:out_transit
                                ~v:transit_attribs' lalr1_transit_attribs in
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
            ) in_attribs_all in
        work io adjs lalr1_transit_attribs workq
      end
  end in
  (* Gather the set of states in conflict-contributing lanes, excluding start states. Start states
   * are always transition sources (never destinations), which makes them trivial to exclude. *)
  let workq = Ordmap.fold ~init:Workq.empty
      ~f:(fun workq (Transit.{dst; _}, _transit_attribs) ->
        match Workq.mem dst workq with
        | true -> workq
        | false -> Workq.push_back dst workq
      ) lalr1_transit_attribs in
  work io adjs lalr1_transit_attribs workq

let close_stable ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs =
  let work_finish io ~stable workq = begin
    (* Remaining queued states are split-stable, because all transitively reachable states in the
     * `ipred_stability_deps` graph are also queued. *)
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
     * (out-transitions may make direct attributions). *)
    Ordset.fold ~init:Attribs.empty
      ~f:(fun in_attribs_all transit ->
        let lane_attribs =
          Ordmap.get_hlt transit lalr1_transit_attribs
          |> TransitAttribs.all in
        Attribs.union lane_attribs in_attribs_all
      ) in_transits_all
  end in
  let is_split_unstable_self symbols prods ~lalr1_transit_attribs
      Attrib.{conflict_state_index; symbol_index; _} in_transits_relevant = begin
    (* Self-contributing conflict state. *)
    let manifestation =
      Ordset.fold ~init:Contrib.empty
        ~f:(fun manifestation in_transit ->
          let Attrib.{contrib; _} =
            Ordmap.get_hlt in_transit lalr1_transit_attribs
            |> TransitAttribs.all
            |> Attribs.get_hlt ~conflict_state_index symbol_index
          in
          Contrib.union contrib manifestation
        ) in_transits_relevant in
    let unsplit_resolution = match resolve with
      | false -> manifestation
      | true -> Contrib.resolve symbols prods symbol_index manifestation
    in
    (* For all relevant in-transitions considered in turn as if the state were split from all other
     * in-transitions, the state is split-stable if the resolution of the in-contribution is the
     * same as the non-split case. *)
    let split_unstable = Ordset.for_any ~f:(fun in_transit ->
      let Attrib.{contrib; _} =
        Ordmap.get_hlt in_transit lalr1_transit_attribs
        |> TransitAttribs.all
        |> Attribs.get_hlt ~conflict_state_index symbol_index
      in
      let split_resolution = match resolve with
        | false -> contrib
        | true -> Contrib.resolve symbols prods symbol_index contrib
      in
      Contrib.(split_resolution <> unsplit_resolution)
    ) in_transits_relevant in
    split_unstable
  end in
  let is_split_unstable ~resolve symbols prods ~lalr1_transit_attribs state_index
      (Attrib.{conflict_state_index; symbol_index; conflict; _} as attrib) in_transits_relevant
      out_transits_relevant = begin
    match State.Index.(conflict_state_index = state_index) with
    | true ->
      is_split_unstable_self symbols prods ~lalr1_transit_attribs attrib in_transits_relevant
    | false -> begin
        (* For all relevant in-transitions considered in turn as if the state were split from all
         * other in-transitions, the state is split-stable if direct-stable and indirect-stable. *)
        let in_direct_contrib_union = Ordset.fold ~init:Contrib.empty
            ~f:(fun in_direct_contrib_union in_transit ->
              let Attrib.{contrib; _} =
                Ordmap.get in_transit lalr1_transit_attribs
                |> Option.value ~default:TransitAttribs.empty
                |> TransitAttribs.direct
                |> Attribs.get ~conflict_state_index symbol_index
                |> Option.value ~default:(Attrib.empty ~conflict_state_index ~symbol_index
                    ~conflict)
              in
              Contrib.union contrib in_direct_contrib_union
            ) in_transits_relevant in
        let split_unstable = Ordset.for_any ~f:(fun in_transit ->
          let Attrib.{contrib=in_contrib; _} =
            Ordmap.get_hlt in_transit lalr1_transit_attribs
            |> TransitAttribs.all
            |> Attribs.get_hlt ~conflict_state_index symbol_index
          in
          Ordset.for_any ~f:(fun out_transit ->
            let Attrib.{contrib=out_contrib; _} =
              Ordmap.get_hlt out_transit lalr1_transit_attribs
              |> TransitAttribs.all
              |> Attribs.get_hlt ~conflict_state_index symbol_index
            in
            let Attrib.{contrib=out_direct_contrib; _} =
              Ordmap.get out_transit lalr1_transit_attribs
              |> Option.value ~default:TransitAttribs.empty
              |> TransitAttribs.direct
              |> Attribs.get ~conflict_state_index symbol_index
              |> Option.value ~default:(Attrib.empty ~conflict_state_index ~symbol_index ~conflict)
            in
            let split_out_contrib = Contrib.(
              union (inter in_contrib out_contrib) out_direct_contrib
            ) in
            let unsplit_out_contrib =
              Contrib.union split_out_contrib in_direct_contrib_union in
            (* 1) Direct-stable: All out-transition sets must resolve the same as if all direct
             *    in-contributions were made. *)
            let split_resolution = match resolve with
              | false -> split_out_contrib
              | true -> Contrib.resolve symbols prods symbol_index split_out_contrib
            in
            let unsplit_resolution_direct = match resolve with
              | false -> unsplit_out_contrib
              | true -> Contrib.resolve symbols prods symbol_index unsplit_out_contrib
            in
            let direct_unstable =
              not Contrib.(split_resolution = unsplit_resolution_direct) in
            (* 2) Indirect-stable: The state is split-stable if the resolution of the
             *    out-contributions is either:
             *    - The same as the non-split case.
             *    - Empty (i.e. the out-transition is not part of a relevant lane).
            *)
            let unsplit_resolution_indirect = match resolve with
              | false -> out_contrib
              | true -> Contrib.resolve symbols prods symbol_index out_contrib
            in
            let indirect_unstable = not Contrib.(
              is_empty split_resolution ||
              split_resolution = unsplit_resolution_indirect
            ) in
            direct_unstable || indirect_unstable
          ) out_transits_relevant
        ) in_transits_relevant in
        split_unstable
      end
  end in
  let gather_ipred_stability_deps_indexes ~resolve symbols prods ~lalr1_transit_attribs ~stable
      ~ipred_stability_deps ~unstable state_index ipred_split_unstable ipred_stability_deps_indexes
      Attrib.{conflict_state_index; symbol_index; _} in_transits_relevant = begin
    (* If ipred is potentially split-unstable, the resolution of `contrib` must have stable
     * resolution for all possible contrib subsets to recognize state as unconditionally
     * split-stable. *)
    let ipred_split_unstable, ipred_stability_deps_indexes = Ordset.fold_until
        ~init:(ipred_split_unstable, ipred_stability_deps_indexes)
        ~f:(fun (ipred_split_unstable, ipred_stability_deps_indexes)
          (Transit.{src=ipred_state_index; _} as in_transit) ->
          let is_ipred_stable = Set.mem ipred_state_index stable in
          let is_ipred_unstable = Set.mem ipred_state_index unstable in
          let is_resolution_unstable = match is_ipred_stable with
            | true -> false
            | false -> begin
                match Ordmap.get state_index ipred_stability_deps with
                | Some ipred_stability_deps_indexes_prev ->
                  (* Use previously computed value. *)
                  Ordset.mem ipred_state_index ipred_stability_deps_indexes_prev
                | None -> begin
                    let Attrib.{contrib; _} =
                      Ordmap.get_hlt in_transit lalr1_transit_attribs
                      |> TransitAttribs.all
                      |> Attribs.get_hlt ~conflict_state_index symbol_index
                    in
                    not (Contrib.stable ~resolve symbols prods symbol_index contrib)
                  end
              end
          in
          let ipred_split_unstable, ipred_stability_deps_indexes =
            match is_resolution_unstable, is_ipred_unstable with
            | false, _ -> ipred_split_unstable, ipred_stability_deps_indexes
            | true, false -> begin
                (* Split-stability depends on the ipred being split-stable, and the ipred's
                 * split-stability is currently undetermined. Record the dependency on the ipred and
                 * requeue. *)
                ipred_split_unstable, Ordset.insert ipred_state_index ipred_stability_deps_indexes
              end
            | true, true -> begin
                (* Split-stability depends on the ipred being split-stable, and the ipred is already
                 * known to be split-unstable. *)
                true, ipred_stability_deps_indexes
              end
          in
          (ipred_split_unstable, ipred_stability_deps_indexes), ipred_split_unstable
        ) in_transits_relevant
    in
    (ipred_split_unstable, ipred_stability_deps_indexes)
  end in
  let gather_isucc_stability_deps_indexes ~stable ~unstable state_index isucc_split_unstable
      isucc_stability_deps_indexes Attrib.{conflict_state_index; _} out_transits_relevant = begin
    let isucc_split_unstable, isucc_stability_deps_indexes =
      match State.Index.(conflict_state_index = state_index) with
      | true -> (true, isucc_stability_deps_indexes)
      | false -> begin
          Ordset.fold_until
            ~init:(isucc_split_unstable, isucc_stability_deps_indexes)
            ~f:(fun (isucc_split_unstable, isucc_stability_deps_indexes)
              Transit.{dst=isucc_state_index; _} ->
              let is_isucc_stable = Set.mem isucc_state_index stable in
              let is_isucc_unstable = Set.mem isucc_state_index unstable in
              let isucc_split_unstable, isucc_stability_deps_indexes =
                match is_isucc_stable, is_isucc_unstable with
                | false, false ->
                  isucc_split_unstable, Ordset.insert isucc_state_index isucc_stability_deps_indexes
                | false, true -> true, isucc_stability_deps_indexes
                | true, false -> isucc_split_unstable, isucc_stability_deps_indexes
                | true, true -> not_reached ()
              in
              (isucc_split_unstable, isucc_stability_deps_indexes), isucc_split_unstable
            ) out_transits_relevant
        end
    in
    (isucc_split_unstable, isucc_stability_deps_indexes)
  end in
  let rec work ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs
      ~stable ~ipred_stability_deps ~isucc_stability_deps ~unstable churn workq = begin
    (* Terminate work if all workq items have been considered twice since the last forward progress
     * (first to try to categorize as stable/unstable, second to break a dependency cycle). This
     * conveniently also terminates if the workq empties. *)
    let workq_length = Workq.length workq in
    match Uns.(churn < workq_length * 2L) with
    | false -> work_finish io ~stable workq
    | true -> begin
        assert (not (Workq.is_empty workq));
        let state_index, workq = Workq.pop workq in
        let in_transits_all, out_transits_all =
          gather_transits state_index lalr1_transit_attribs adjs in
        let in_attribs_all = gather_in_attribs lalr1_transit_attribs in_transits_all in
        (* Test whether this state is split-stable with respect to each attribution. There are three
         * possible outcomes:
         * - The state is split-stable.
         * - The state is split-unstable.
         * - The state may be split-stable, if all its ipreds/isuccs are determined to be
         *   split-stable (i.e. ipred/isucc-dependent split-stability). *)
        let split_unstable, ipred_split_unstable, isucc_split_unstable,
          ipred_stability_deps_indexes, isucc_stability_deps_indexes =
          Attribs.fold ~init:(false, false, false, Ordset.empty (module State.Index),
            Ordset.empty (module State.Index))
            ~f:(fun (split_unstable, ipred_split_unstable, isucc_split_unstable,
              ipred_stability_deps_indexes, isucc_stability_deps_indexes)
              (Attrib.{conflict_state_index; symbol_index; _} as attrib) ->
              let in_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  in_transits_all ~conflict_state_index symbol_index in
              let out_transits_relevant = filter_transits_relevant lalr1_transit_attribs
                  out_transits_all ~conflict_state_index symbol_index in
              let split_unstable = match split_unstable with
                | true -> true
                | false -> begin
                    is_split_unstable ~resolve symbols prods ~lalr1_transit_attribs
                      state_index attrib in_transits_relevant out_transits_relevant
                  end
              in
              let ipred_split_unstable, ipred_stability_deps_indexes =
                gather_ipred_stability_deps_indexes ~resolve symbols prods
                  ~lalr1_transit_attribs ~stable ~ipred_stability_deps ~unstable state_index
                  ipred_split_unstable ipred_stability_deps_indexes attrib
                  in_transits_relevant
              in
              let isucc_split_unstable, isucc_stability_deps_indexes =
                gather_isucc_stability_deps_indexes ~stable ~unstable state_index
                  isucc_split_unstable isucc_stability_deps_indexes attrib
                  out_transits_relevant
              in
              (split_unstable, ipred_split_unstable, isucc_split_unstable,
                ipred_stability_deps_indexes, isucc_stability_deps_indexes)
            ) in_attribs_all
        in
        let open Tristate in
        let ipred_split_unstable_tri =
          match ipred_split_unstable, Ordset.is_empty ipred_stability_deps_indexes with
          | false, true -> No
          | false, false -> Maybe
          | true, _ -> Yes
        in
        let isucc_split_unstable_tri =
          match isucc_split_unstable, Ordset.is_empty isucc_stability_deps_indexes with
          | false, true -> No
          | false, false -> Maybe
          | true, _ -> Yes
        in
        let io, stable, ipred_stability_deps, isucc_stability_deps, unstable, churn, workq =
          let break_cycle = Uns.(churn >= workq_length) in
          match split_unstable, ipred_split_unstable_tri, isucc_split_unstable_tri, break_cycle with
          | false, No, _, _
          | true, No, No, _ -> begin
              (* Split-stable. *)
              io.log |> Fmt.fmt "." |> Io.with_log io,
              Set.insert state_index stable,
              Ordmap.remove state_index ipred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              unstable,
              0L,
              workq
            end
          | _, Yes, _, _
          | true, (No|Maybe), Yes, _
          | true, (No|Maybe), Maybe, true -> begin
              (* Split-unstable, or breaking cycle. *)
              io.log |> Fmt.fmt "^" |> Io.with_log io,
              stable,
              Ordmap.remove state_index ipred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              Set.insert state_index unstable,
              0L,
              workq
            end
          | _, Maybe, No, _
          | false, Maybe, Yes, _ -> begin
              (* Possible ipred-dependent split-stability. *)
              io,
              stable,
              Ordmap.upsert ~k:state_index ~v:ipred_stability_deps_indexes ipred_stability_deps,
              Ordmap.remove state_index isucc_stability_deps,
              unstable,
              succ churn,
              Workq.push_back state_index workq
            end
          | false, Maybe, Maybe, _
          | true, Maybe, Maybe, false -> begin
              (* Possible ipred/isucc-dependent split-stability. *)
              io,
              stable,
              Ordmap.upsert ~k:state_index ~v:ipred_stability_deps_indexes ipred_stability_deps,
              Ordmap.upsert ~k:state_index ~v:isucc_stability_deps_indexes isucc_stability_deps,
              unstable,
              succ churn,
              Workq.push_back state_index workq
            end
          | true, No, Maybe, false -> begin
              (* Possible isucc-dependent split-stability. *)
              io,
              stable,
              Ordmap.remove state_index ipred_stability_deps,
              Ordmap.upsert ~k:state_index ~v:isucc_stability_deps_indexes isucc_stability_deps,
              unstable,
              succ churn,
              Workq.push_back state_index workq
            end
        in
        work ~resolve io symbols prods lalr1_isocores lalr1_states adjs ~lalr1_transit_attribs
          ~stable ~ipred_stability_deps ~isucc_stability_deps ~unstable churn workq
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
      ~ipred_stability_deps:(Ordmap.empty (module State.Index))
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
            let conflict_state_index = State.index state in
            let lalr1_transit_attribs =
              gather_transit_attribs ~resolve symbols prods lalr1_states adjs
                ~lalr1_transit_attribs conflict_state_index in
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
  let gotonub_of_statenub_goto statenub goto = begin
    let transit = transit_of_statenub_goto statenub goto in
    let transit_attribs = match Ordmap.get transit lalr1_transit_attribs with
      | None -> TransitAttribs.empty
      | Some transit_attribs -> transit_attribs
    in
    GotoNub.init ~goto ~transit_attribs
  end in
  io, gotonub_of_statenub_goto
