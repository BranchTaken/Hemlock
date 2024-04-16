open Basis
open! Basis.Rudiments

let annotations_init ~resolve io symbols prods lalr1_isocores lalr1_states =
  let gather_transit_contribs ~resolve prods lalr1_states antes ~lalr1_transit_contribs
      conflict_state_index = begin
    (* Backpropagate contribs that were directly attributed, such that all lane antecedents make
     * equivalent indirect contribs. *)
    let rec backprop_transit_contribs antes transit_contribs lalr1_transit_contribs marks
        state_index = begin
      Array.fold ~init:lalr1_transit_contribs
        ~f:(fun lalr1_transit_contribs ante_state_index ->
          match Set.mem ante_state_index marks with
          | true -> lalr1_transit_contribs
          | false -> begin
              let transit = Transit.init ~src:ante_state_index ~dst:state_index in
              assert (not (Transit.cyclic transit));
              let transit_contribs_prev = match Ordmap.get transit lalr1_transit_contribs with
                | None -> TransitContribs.empty
                | Some transit_contribs_prev -> transit_contribs_prev
              in
              let transit_contribs_union =
                TransitContribs.union transit_contribs transit_contribs_prev in
              match AnonContribs.equal (TransitContribs.all transit_contribs_union)
                (TransitContribs.all transit_contribs_prev) with
              | true -> lalr1_transit_contribs
              | false -> begin
                  let lalr1_transit_contribs = Ordmap.upsert ~k:transit ~v:transit_contribs_union
                      lalr1_transit_contribs in
                  let marks = Set.insert ante_state_index marks in
                  backprop_transit_contribs antes transit_contribs lalr1_transit_contribs marks
                    ante_state_index
                end
            end
        ) (Antes.antes_of_state_index state_index antes)
    end in
    let rec ante_transit_contribs ~resolve lalr1_states antes ~lalr1_transit_contribs marks
        lanectx = begin
      (* Marking of the current lane segment spanning the start state back to the current state
       * prevents infinite recursion. It is possible for a grammar to induce a combinatorial
       * explosion of contributing lanes, but only non-redundant transition contribs lead to
       * recursion, thus assuring that each transition is recursed on only once. *)
      let state = LaneCtx.state lanectx in
      let state_index = State.index state in
      assert (not (Set.mem state_index marks));
      let marks = Set.insert state_index marks in
      (* Accumulate transit contribs and antecedents of `lanectx`. *)
      let lalr1_transit_contribs, ante_lanectxs =
        Array.fold ~init:(lalr1_transit_contribs, [])
          ~f:(fun (lalr1_transit_contribs, ante_lanectxs) ante_state_index ->
            let ante_state = Array.get ante_state_index lalr1_states in
            let ante_lanectx = LaneCtx.of_ante ante_state lanectx in
            let ante_kernel_contribs = LaneCtx.kernel_contribs ante_lanectx in
(*
            File.Fmt.stderr |> Fmt.fmt "XXX ante_lanectx=" |> LaneCtx.fmt_hr ~alt:true symbols prods ante_lanectx |> Fmt.fmt "\n" |> ignore;
*)
            let transit = LaneCtx.transit ante_lanectx in
            (* Load current transit contribs. It is possible for there to be existing contribs to
             * other conflict states. *)
            let transit_contribs =
              Ordmap.get transit lalr1_transit_contribs
              |> Option.value ~default:TransitContribs.empty
            in
            let kernel_contribs = TransitContribs.kernel_contribs transit_contribs in
            let transit_contribs' =
              TransitContribs.insert_kernel_contribs ante_kernel_contribs transit_contribs in
            (* Avoid recursing if no new transit contribs were inserted, since no additional
             * insertions will occur in the recursion. *)
            let kernel_contribs' = TransitContribs.kernel_contribs transit_contribs' in
            let lalr1_transit_contribs =
              match KernelContribs.equal kernel_contribs' kernel_contribs with
              | true -> lalr1_transit_contribs
              | false -> begin
                  assert (not (Transit.cyclic transit));
                  let lalr1_transit_contribs =
                    Ordmap.upsert ~k:transit ~v:transit_contribs' lalr1_transit_contribs in
                  (* Recurse if lanes may extend to antecedents. *)
                  match LaneCtx.traces_length ante_lanectx with
                  | 0L -> lalr1_transit_contribs
                  | _ -> ante_transit_contribs ~resolve lalr1_states antes ~lalr1_transit_contribs
                      marks ante_lanectx
                end
            in
            let ante_lanectxs = ante_lanectx :: ante_lanectxs in
            lalr1_transit_contribs, ante_lanectxs
          ) (Array.filter ~f:(fun ante_state_index -> not (Set.mem ante_state_index marks))
            (Antes.antes_of_state_index state_index antes))
      in
      (* Finish computing direct attributions for `lanectx`. This is done post-order to detect
       * attributions for which there is a relevant kernel item in `lanectx`, but no relevant item
       * in any of its antecedents' lane contexts. *)
      let lanectx = LaneCtx.post_init ante_lanectxs lanectx in
(*
      File.Fmt.stderr |> Fmt.fmt "XXX post_init lanectx=" |> LaneCtx.fmt_hr ~alt:true symbols prods lanectx |> Fmt.fmt "\n" |> ignore;
*)
      (* Accumulate direct attributions. *)
      let transit = LaneCtx.transit lanectx in
      let anon_contribs_direct = LaneCtx.anon_contribs_direct lanectx in
      let lalr1_transit_contribs = match AnonContribs.is_empty anon_contribs_direct with
        | true -> lalr1_transit_contribs
        | false -> begin
            (* Backpropagate. *)
            let transit_contribs = TransitContribs.of_anon_contribs anon_contribs_direct in
            let lalr1_transit_contribs = backprop_transit_contribs antes transit_contribs
                lalr1_transit_contribs marks state_index in
            let lalr1_transit_contribs = match Transit.cyclic transit with
              | true -> lalr1_transit_contribs
              | false -> begin
                  let transit_contribs_direct =
                    TransitContribs.of_anon_contribs_direct anon_contribs_direct in
                  Ordmap.amend transit ~f:(function
                    | None -> Some transit_contribs_direct
                    | Some transit_contribs_existing ->
                      Some (TransitContribs.union transit_contribs_direct transit_contribs_existing)
                  ) lalr1_transit_contribs
                end
            in
            lalr1_transit_contribs
          end
      in
      lalr1_transit_contribs
    end in
    let marks = Set.empty (module State.Index) in
    let conflict_state = Array.get conflict_state_index lalr1_states in
    let lanectx = LaneCtx.of_conflict_state ~resolve symbols prods conflict_state in
    ante_transit_contribs ~resolve lalr1_states antes ~lalr1_transit_contribs marks lanectx
  end in
  let close_transit_contribs io antes ergos lalr1_transit_contribs = begin
    (* Propagate contribs forward wherever possible, until no further propagation is possible. A
     * contrib can be propagated forward if all in-transitions with relevant attributions make the
     * same contribution.
     *
     * Note that propagation is on a per attribution basis, and for each propagation attempt, only
     * in/out-transitions with the relevant {conflict state, symbol} are considered and propagated
     * from/to. *)
    let rec work io antes ergos lalr1_transit_contribs workq = begin
      match Workq.is_empty workq with
      | true -> io, lalr1_transit_contribs
      | false -> begin
          let io = io.log |> Fmt.fmt "." |> Io.with_log io in
          let state_index, workq = Workq.pop workq in
          (* Filter in/out transits for which there are no conflict contributions, since they lie
           * outside any relevant lane. *)
          let in_transits = Array.fold ~init:(Ordset.empty (module Transit))
            ~f:(fun in_transits ante_state_index ->
              let transit = Transit.init ~src:ante_state_index ~dst:state_index in
              match Ordmap.get transit lalr1_transit_contribs with
              | None -> in_transits
              | Some _ -> Ordset.insert transit in_transits
            ) (Antes.antes_of_state_index state_index antes) in
          let out_transits = Array.fold ~init:(Ordset.empty (module Transit))
            ~f:(fun out_transits ergo_state_index ->
              let transit = Transit.init ~src:state_index ~dst:ergo_state_index in
              match Ordmap.get transit lalr1_transit_contribs with
              | None -> out_transits
              | Some _ -> Ordset.insert transit out_transits
            ) (Ergos.ergos_of_state_index state_index ergos) in
          let in_contribs_all = Ordset.fold ~init:AnonContribs.empty
              ~f:(fun contribs_all transit ->
                let anon_contribs =
                  Ordmap.get_hlt transit lalr1_transit_contribs
                  |> TransitContribs.all in
                AnonContribs.union anon_contribs contribs_all
              ) in_transits in
          let io, lalr1_transit_contribs, workq =
            AnonContribs.fold ~init:(io, lalr1_transit_contribs, workq)
              ~f:(fun (io, lalr1_transit_contribs, workq) conflict_state_index symbol_index
                in_contrib_all ->
                (* Filter in/out transits lacking the relevant {conflict_state, symbol}. *)
                let in_transits_relevant = Ordset.filter ~f:(fun in_transit ->
                  Ordmap.get_hlt in_transit lalr1_transit_contribs
                  |> TransitContribs.all
                  |> AnonContribs.get ~conflict_state_index symbol_index
                  |> Option.is_some
                ) in_transits in
                let out_transits_relevant = Ordset.filter ~f:(fun out_transit ->
                  Ordmap.get_hlt out_transit lalr1_transit_contribs
                  |> TransitContribs.all
                  |> AnonContribs.get ~conflict_state_index symbol_index
                  |> Option.is_some
                ) out_transits in
                (* Determine whether there exists a common in-contrib, the existence of which
                 * allows propagation. *)
                let out_contrib_all = Ordset.fold_until ~init:in_contrib_all
                    ~f:(fun out_contrib_all in_transit ->
                      let contrib =
                        Ordmap.get_hlt in_transit lalr1_transit_contribs
                        |> TransitContribs.all
                        |> AnonContribs.get_hlt ~conflict_state_index symbol_index in
                      let out_contrib_all = Contrib.inter contrib out_contrib_all in
                      out_contrib_all, Contrib.is_empty out_contrib_all
                    ) in_transits_relevant in
                let io, lalr1_transit_contribs, workq =
                  match Contrib.is_empty out_contrib_all with
                  | true -> io, lalr1_transit_contribs, workq
                  | false -> begin
                      (* Propagate forward. *)
                      Ordset.fold ~init:(io, lalr1_transit_contribs, workq)
                        ~f:(fun (io, lalr1_transit_contribs, workq) out_transit ->
                          let transit_contribs =
                            Ordmap.get_hlt out_transit lalr1_transit_contribs in
                          let transit_contribs' = TransitContribs.merge ~conflict_state_index
                              symbol_index out_contrib_all transit_contribs in
                          match AnonContribs.equal (TransitContribs.all transit_contribs')
                            (TransitContribs.all transit_contribs) with
                          | true -> io, lalr1_transit_contribs, workq
                          | false -> begin
                              let lalr1_transit_contribs = Ordmap.update_hlt ~k:out_transit
                                  ~v:transit_contribs' lalr1_transit_contribs in
                              let ergo_state_index = Transit.(out_transit.dst) in
                              let io, workq = match Workq.mem ergo_state_index workq with
                                | true -> io, workq
                                | false -> begin
                                    let io = io.log |> Fmt.fmt "+" |> Io.with_log io in
                                    io, Workq.push_back ergo_state_index workq
                                  end
                              in
                              io, lalr1_transit_contribs, workq
                            end
                        ) out_transits_relevant
                    end
                in
                io, lalr1_transit_contribs, workq
              ) in_contribs_all in
          work io antes ergos lalr1_transit_contribs workq
        end
    end in
    (* Gather the set of states in conflict-contributing lanes, excluding start states. Start
     * states are always transition sources (never destinations), which makes them trivial to
     * exclude. *)
    let workq = Ordmap.fold ~init:Workq.empty
        ~f:(fun workq (Transit.{dst; _}, _transit_contribs) ->
          match Workq.mem dst workq with
          | true -> workq
          | false -> Workq.push_back dst workq
        ) lalr1_transit_contribs in
    work io antes ergos lalr1_transit_contribs workq
  end in
  let close_stable ~resolve io symbols prods lalr1_isocores lalr1_states antes ergos
      ~lalr1_transit_contribs = begin
    let rec work ~resolve io symbols prods lalr1_isocores lalr1_states antes ergos
        ~lalr1_transit_contribs ~stable stability_deps ~unstable churn workq = begin
      (* Terminate work if all workq items have been considered since the last forward progress.
       * This conveniently also terminates if the workq empties. *)
      match churn < Workq.length workq with
      | false -> begin
          (* Remaining queued states are split-stable, becauses all transitively reachable states
           * in the `stability_deps` graph are also qeued. *)
          let reqd = Workq.set workq in
(*
          File.Fmt.stderr |> Fmt.fmt "XXX stability_deps=" |> Ordmap.fmt ~alt:true Ordset.pp stability_deps |> Fmt.fmt "\n" |> ignore;
          File.Fmt.stderr |> Fmt.fmt "XXX reqd=" |> Set.pp reqd |> Fmt.fmt "\n" |> ignore;
          File.Fmt.stderr |> Fmt.fmt "XXX unstable=" |> Set.pp unstable |> Fmt.fmt "\n" |> ignore;
*)
          let io =
            io.log
            |> String.fmt ~pad:(Codepoint.of_char '.') ~width:(Set.length reqd) ""
            |> Io.with_log io
          in
          let stable = Set.union reqd stable in
          io, stable
        end
      | true -> begin
          assert (not (Workq.is_empty workq));
          let state_index, workq = Workq.pop workq in
(*
          File.Fmt.stderr |> Fmt.fmt "XXX work churn=" |> Uns.pp churn |> Fmt.fmt ", state_index=" |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore;
*)
          (* Filter in/out transits for which there are no conflict contributions, since they lie
           * outside any relevant lane. *)
          let in_transits_all = Array.fold ~init:(Ordset.empty (module Transit))
            ~f:(fun in_transits_all ante_state_index ->
              let transit = Transit.init ~src:ante_state_index ~dst:state_index in
              match Ordmap.get transit lalr1_transit_contribs with
              | None -> in_transits_all
              | Some _ -> Ordset.insert transit in_transits_all
            ) (Antes.antes_of_state_index state_index antes) in
          let out_transits_all = Array.fold ~init:(Ordset.empty (module Transit))
            ~f:(fun out_transits_all ergo_state_index ->
              let transit = Transit.init ~src:state_index ~dst:ergo_state_index in
              match Ordmap.get transit lalr1_transit_contribs with
              | None -> out_transits_all
              | Some _ -> Ordset.insert transit out_transits_all
            ) (Ergos.ergos_of_state_index state_index ergos) in
          (* Gather the set of all in-contribs, which is a non-strict subset of all out-contribs
           * (out-transitions may make direct contributions. *)
          let in_contribs_all = Ordset.fold ~init:AnonContribs.empty
              ~f:(fun in_contribs_all transit ->
                let anon_contribs = Ordmap.get_hlt transit lalr1_transit_contribs
                                    |> TransitContribs.all in
                AnonContribs.union anon_contribs in_contribs_all
              ) in_transits_all in
          (* Iteratively test whether this state is split-stable with respect to each
           * contribution. There are three possible outcomes:
           * - The state is split-stable.
           * - The state is split-unstable.
           * - The state may be split-stable, but only if one or more of its antecedents are
           *   determined to be split-stable (i.e. antecedent-dependent split-stability). *)
          let io, stability_deps_indexes_opt = AnonContribs.fold_until
              ~init:(io, Some (Ordset.empty (module State.Index)))
              ~f:(fun (io, stability_deps_indexes_opt) conflict_state_index symbol_index
                _contrib_all ->
                (* Filter in/out transits lacking the relevant {conflict_state, symbol}. *)
                let in_transits_relevant = Ordset.filter ~f:(fun in_transit ->
                  Ordmap.get_hlt in_transit lalr1_transit_contribs
                  |> TransitContribs.all
                  |> AnonContribs.get ~conflict_state_index symbol_index
                  |> Option.is_some
                ) in_transits_all in
                (* If state has already been evaluated as antecedent-dependent there is no need to
                 * re-evaluate independent split-stability. *)
                let has_stability_deps = Ordmap.mem state_index stability_deps in
                let io, split_unstable = match has_stability_deps with
                  | true -> io, false
                  | false -> begin
                      match conflict_state_index = state_index with
                      | true -> begin
                          (* Self-contributing conflict state. *)
                          let manifestation =
                            Ordset.fold ~init:Contrib.empty
                              ~f:(fun manifestation in_transit ->
                                let aval =
                                  Ordmap.get_hlt in_transit lalr1_transit_contribs
                                  |> TransitContribs.all
                                  |> AnonContribs.get_hlt ~conflict_state_index symbol_index
                                in
                                Contrib.union aval manifestation
                              ) in_transits_relevant in
                          let unsplit_resolution = match resolve with
                            | false -> manifestation
                            | true -> Contrib.resolve symbols prods symbol_index manifestation
                          in
                          (* For all relevant in-transitions considered in turn as if the state
                           * were split from all other in-transitions, the state is split-stable
                           * if the resolution of the in-contribution is the same as the non-split
                           * case. *)
                          let split_unstable = Ordset.for_any ~f:(fun in_transit ->
                            let contrib =
                              Ordmap.get_hlt in_transit lalr1_transit_contribs
                              |> TransitContribs.all
                              |> AnonContribs.get_hlt ~conflict_state_index symbol_index
                            in
                            let split_resolution = match resolve with
                              | false -> contrib
                              | true -> Contrib.resolve symbols prods symbol_index contrib
                            in
                            Contrib.(split_resolution <> unsplit_resolution)
                          ) in_transits_relevant in
(*
                          let () = match split_unstable with
                            | false -> ()
                            | true -> File.Fmt.stderr |> Fmt.fmt "XXX self-unstable: " |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore
                          in
*)
                          io, split_unstable
                        end
                      | false -> begin
                          (* For all relevant in-transitions considered in turn as if the state
                           * were split from all other in-transitions, the state is split-stable
                           * if direct-stable and indirect-stable. *)
                          let out_transits_relevant = Ordset.filter ~f:(fun out_transit ->
                            Ordmap.get_hlt out_transit lalr1_transit_contribs
                            |> TransitContribs.all
                            |> AnonContribs.get ~conflict_state_index symbol_index
                            |> Option.is_some
                          ) out_transits_all in
                          let in_direct_contrib_union = Ordset.fold ~init:Contrib.empty
                              ~f:(fun in_direct_contrib_union in_transit ->
                                let contrib =
                                  Ordmap.get in_transit lalr1_transit_contribs
                                  |> Option.value ~default:TransitContribs.empty
                                  |> TransitContribs.direct
                                  |> AnonContribs.get ~conflict_state_index symbol_index
                                  |> Option.value ~default:Contrib.empty
                                in
                                Contrib.union contrib in_direct_contrib_union
                              ) in_transits_relevant in
                          let split_unstable = Ordset.for_any ~f:(fun in_transit ->
                            let in_contrib =
                              Ordmap.get_hlt in_transit lalr1_transit_contribs
                              |> TransitContribs.all
                              |> AnonContribs.get_hlt ~conflict_state_index symbol_index
                            in
                            Ordset.for_any ~f:(fun out_transit ->
                              let out_contrib =
                                Ordmap.get_hlt out_transit lalr1_transit_contribs
                                |> TransitContribs.all
                                |> AnonContribs.get_hlt ~conflict_state_index symbol_index
                              in
                              let out_direct_contrib =
                                Ordmap.get out_transit lalr1_transit_contribs
                                |> Option.value ~default:TransitContribs.empty
                                |> TransitContribs.direct
                                |> AnonContribs.get ~conflict_state_index symbol_index
                                |> Option.value ~default:Contrib.empty
                              in
                              let split_out_contrib = Contrib.(
                                union (inter in_contrib out_contrib) out_direct_contrib
                              ) in
                              let unsplit_out_contrib =
                                Contrib.union split_out_contrib in_direct_contrib_union in
                              (* 1) Direct-stable: All out-transition sets must resolve the same
                               *    as if all direct in-contributions were made. *)
                              let split_resolution = match resolve with
                                | false -> split_out_contrib
                                | true ->
                                  Contrib.resolve symbols prods symbol_index split_out_contrib
                              in
                              let unsplit_resolution_direct = match resolve with
                                | false -> unsplit_out_contrib
                                | true ->
                                  Contrib.resolve symbols prods symbol_index unsplit_out_contrib
                              in
                              let direct_unstable =
                                not Contrib.(split_resolution = unsplit_resolution_direct) in
                              (* 2) Indirect-stable: The state is split-stable if the resolution
                               *    of the out-contributions is either:
                               *    - The same as the non-split case.
                               *    - Empty (i.e. the out-transition is not part of a relevant
                               *      lane). *)
                              let unsplit_resolution_indirect = match resolve with
                                | false -> out_contrib
                                | true -> Contrib.resolve symbols prods symbol_index out_contrib
                              in
                              let indirect_unstable = not Contrib.(
                                is_empty split_resolution ||
                                split_resolution = unsplit_resolution_indirect
                              ) in
(*
                              let () = match direct_unstable with
                                | false -> ()
                                | true -> File.Fmt.stderr |> Fmt.fmt "XXX direct-unstable: " |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore
                              in
                              let () = match indirect_unstable with
                                | false -> ()
                                | true -> File.Fmt.stderr |> Fmt.fmt "XXX indirect-unstable: " |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore
                              in
*)
                              direct_unstable || indirect_unstable
                            ) out_transits_relevant
                          ) in_transits_relevant in
                          (io, split_unstable)
                        end
                    end
                in
                match split_unstable with
                | true -> (io, None), true
                | false -> begin
                    (* If antecedent is potentially split-unstable, the resolution of `contrib`
                     * must have stable resolution for all possible contrib subsets to recognize
                     * state as unconditionally split-stable. *)
                    let io, stability_deps_indexes_opt = Ordset.fold_until
                        ~init:(io, stability_deps_indexes_opt)
                        ~f:(fun (io, stability_deps_indexes_opt) in_transit ->
                          let ante_state_index = Transit.(in_transit.src) in
                          let is_ante_stable = Set.mem ante_state_index stable in
                          let is_ante_unstable = Set.mem ante_state_index unstable in
                          let is_resolution_unstable = match is_ante_stable with
                            | true -> false
                            | false -> begin
                                match Ordmap.get state_index stability_deps with
                                | Some stability_deps_indexes_prev ->
                                  (* Use previously computed value. *)
                                  Ordset.mem ante_state_index stability_deps_indexes_prev
                                | None -> begin
                                    let contrib =
                                      Ordmap.get_hlt in_transit lalr1_transit_contribs
                                      |> TransitContribs.all
                                      |> AnonContribs.get_hlt ~conflict_state_index symbol_index
                                    in
                                    not (Contrib.stable ~resolve symbols prods symbol_index
                                        contrib)
                                  end
                              end
                          in
                          match is_resolution_unstable, is_ante_unstable with
                          | false, _ -> (io, stability_deps_indexes_opt), false
                          | true, false -> begin
                              (* Split-stability depends on the antecedent being split-stable, and
                               * the antecedent's split-stability is currently undetermined.
                               * Record the dependency on the antecedent and requeue. The
                               * dependency information only comes into play if the work queue
                               * fails to determine split-stability of all states, as can happen
                               * with dependency cycles. *)
                              let stability_deps_indexes =
                                Option.value ~default:(Ordset.empty (module State.Index))
                                  stability_deps_indexes_opt in
(*
                              File.Fmt.stderr |> Fmt.fmt "XXX maybe antecedent-unstable: " |> State.Index.pp state_index |> Fmt.fmt ", unstable ante_state_index=" |> State.Index.pp ante_state_index |> Fmt.fmt "\n" |> ignore;
*)
                              (io, Some (Ordset.insert ante_state_index stability_deps_indexes)),
                              false
                            end
                          | true, true -> begin
                              (* Split-stability depends on the antecedent being split-stable, and
                               * the antecedent is already known to be split-unstable. Requeuing
                               * would cause no correctness issues, but doing so would cause
                               * pointless extra work. *)
(*
                              File.Fmt.stderr |> Fmt.fmt "XXX antecedent-unstable: " |> State.Index.pp state_index |> Fmt.fmt ", unstable ante_state_index=" |> State.Index.pp ante_state_index |> Fmt.fmt "\n" |> ignore;
*)
                              (io, None), true
                            end
                        ) in_transits_relevant in
                    (io, stability_deps_indexes_opt), Option.is_none stability_deps_indexes_opt
                  end
              ) in_contribs_all in
          let io, stable, stability_deps, unstable, churn, workq =
            match stability_deps_indexes_opt with
            | Some stability_deps_indexes when Ordset.is_empty stability_deps_indexes -> begin
(*
                File.Fmt.stderr |> Fmt.fmt "XXX stable: " |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore;
*)
                io.log |> Fmt.fmt "." |> Io.with_log io,
                Set.insert state_index stable,
                Ordmap.remove state_index stability_deps, (* Removal not strictly necessary. *)
                unstable,
                0L,
                workq
              end
            | None -> begin
(*
                File.Fmt.stderr |> Fmt.fmt "XXX unstable: " |> State.Index.pp state_index |> Fmt.fmt "\n" |> ignore;
*)
                io.log |> Fmt.fmt "^" |> Io.with_log io,
                stable,
                Ordmap.remove state_index stability_deps, (* Removal not strictly necessary. *)
                Set.insert state_index unstable,
                0L,
                workq
              end
            | Some stability_deps_indexes -> begin
(*
                File.Fmt.stderr |> Fmt.fmt "XXX req: " |> State.Index.pp state_index |> Fmt.fmt ", stability_deps_indexes=" |> Ordset.pp stability_deps_indexes |> Fmt.fmt "\n" |> ignore;
*)
                io,
                stable,
                Ordmap.upsert ~k:state_index ~v:stability_deps_indexes stability_deps,
                unstable,
                succ churn,
                Workq.push_back state_index workq
              end
          in
          work ~resolve io symbols prods lalr1_isocores lalr1_states antes ergos
            ~lalr1_transit_contribs ~stable stability_deps
            ~unstable churn workq
        end
    end in
    (* Gather the set of states in conflict-contributing lanes, excluding start states, which are
     * always split-stable. Start states are always transition sources (never destinations), which
     * makes them trivial to exclude. *)
    let workq = Ordmap.fold ~init:Workq.empty
        ~f:(fun workq (Transit.{dst; _}, _contribs) ->
          match Workq.mem dst workq with
          | true -> workq
          | false -> Workq.push_back dst workq
        ) lalr1_transit_contribs in
(*
    File.Fmt.stderr |> Fmt.fmt "XXX workq=" |> Workq.pp workq |> Fmt.fmt "\n" |> ignore;
*)
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
    let io, stable = work ~resolve io symbols prods lalr1_isocores lalr1_states antes ergos
        ~lalr1_transit_contribs
        ~stable
        (Ordmap.empty (module State.Index))
        ~unstable:(Set.empty (module State.Index))
        0L
        workq in
    let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
(*
    File.Fmt.stderr |> Fmt.fmt "XXX stable=" |> Set.pp stable |> Fmt.fmt "\n" |> ignore;
*)
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
  end in

  let antes = Antes.init lalr1_states in
  let ergos = Ergos.init antes in
(*
  File.Fmt.stderr |> Fmt.fmt "XXX antes=" |> Antes.pp antes |> Fmt.fmt "\n" |> ignore;
  File.Fmt.stderr |> Fmt.fmt "XXX ergos=" |> Ergos.pp (Ergos.init antes) |> Fmt.fmt "\n" |> ignore;
*)
  (* Gather transit contribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict contributions"
    |> Io.with_log io
  in
  let io, lalr1_transit_contribs =
    Array.fold ~init:(io, Ordmap.empty (module Transit))
      ~f:(fun (io, lalr1_transit_contribs) state ->
        let conflicts_on =
          State.conflict_attribs ~resolve symbols prods state
          |> Attribs.symbol_indexes
        in
        match Ordset.is_empty conflicts_on with
        | true -> io, lalr1_transit_contribs
        | false -> begin
            let io = io.log |> Fmt.fmt "." |> Io.with_log io in
            let conflict_state_index = State.index state in
            let lalr1_transit_contribs =
              gather_transit_contribs ~resolve prods lalr1_states antes ~lalr1_transit_contribs
                conflict_state_index in
            io, lalr1_transit_contribs
          end
      ) lalr1_states
  in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in

  let io =
    io.log
    |> Fmt.fmt "hocc: Closing IELR(1) conflict contributions"
    |> Io.with_log io
  in
  let io, lalr1_transit_contribs = close_transit_contribs io antes ergos lalr1_transit_contribs in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
(*
  File.Fmt.stderr |> Fmt.fmt "XXX lalr1_transit_contribs="
  |> (Ordmap.fmt ~alt:true (TransitContribs.fmt_hr symbols prods ~alt:true ~width:4L) lalr1_transit_contribs)
  |> Fmt.fmt "\n"
  |> ignore;
*)

  (* Determine state split-stability. *)
  let io, lalr1_isocores_stable = match true (* XXX *) with
    | true -> close_stable ~resolve io symbols prods lalr1_isocores
      lalr1_states antes ergos ~lalr1_transit_contribs
    | false -> io, Set.empty (module Uns)
  in

  (* Filter out transit contribs to split-stable states. *)
  let lalr1_transit_contribs = Ordmap.filter ~f:(fun (Transit.{dst; _}, _transit_contribs) ->
    not (Set.mem dst lalr1_isocores_stable)
  ) lalr1_transit_contribs in
(*
  File.Fmt.stderr |> Fmt.fmt "XXX Filtered lalr1_transit_contribs="
  |> (Ordmap.fmt ~alt:true (TransitContribs.fmt_hr symbols prods ~alt:true ~width:4L) lalr1_transit_contribs)
  |> Fmt.fmt "\n"
  |> ignore;
*)

  io, lalr1_transit_contribs

let gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states =
  (* Create lookup function for contribs that closes on the prerequisite LALR(1) inadequacy
   * analysis. *)
  let io, lalr1_transit_contribs =
    annotations_init ~resolve io symbols prods lalr1_isocores lalr1_states in
  let transit_of_statenub_goto statenub goto = begin
    let statenub_core = (Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure.kernel)) in
    let goto_core = Lr1Itemset.core goto in
    let src = Isocores.get_core_hlt statenub_core lalr1_isocores in
    let dst = Isocores.get_core_hlt goto_core lalr1_isocores in
    Transit.init ~src ~dst
  end in
  let gotonub_of_statenub_goto statenub goto = begin
    let transit = transit_of_statenub_goto statenub goto in
    let transit_contribs = match Ordmap.get transit lalr1_transit_contribs with
      | None -> TransitContribs.empty
      | Some transit_contribs -> transit_contribs
    in
    GotoNub.init ~goto ~transit_contribs
  end in
  io, gotonub_of_statenub_goto
