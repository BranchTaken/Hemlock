open Basis
open! Basis.Rudiments

let rec ipred_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_kernel_attribs
    lanectx =
  let state_index = State.index (LaneCtx.state lanectx) in
  (* Accumulate kernel attribs of ipred lane contexts. *)
  Array.fold ~init:lalr1_kernel_attribs
    ~f:(fun lalr1_kernel_attribs ipred_state_index ->
      let ipred_state = Array.get ipred_state_index lalr1_states in
      let ipred_lanectx = LaneCtx.of_ipred ipred_state lanectx in
      let ipred_kernel_attribs = LaneCtx.kernel_attribs ipred_lanectx in
      let transit = LaneCtx.transit ipred_lanectx in
      (* Load any existing kernel attribs, whether to other conflict states, or as a result of
       * recursing into a lane cycle. *)
      let kernel_attribs =
        Ordmap.get transit lalr1_kernel_attribs
        |> Option.value ~default:KernelAttribs.empty
      in
      (* Manually compute the union of `kernel_attribs` and `ipred_kernel_attribs` such that
       * `do_insert` is false if the union equals `kernel_attribs`, i.e. insertion into
       * `lalr1_kernel_attribs` would be a no-op. The conceptually simpler approach of computing the
       * union via `KernelAttribs.union` and checking equality of before/after kernel attribs is a
       * lot more expensive for the no-op (equal) case. *)
      let do_insert, kernel_attribs' = KernelAttribs.fold ~init:(false, kernel_attribs)
        ~f:(fun (do_insert, kernel_attribs') (lr1item, attribs) ->
          match KernelAttribs.get lr1item kernel_attribs with
          | None -> true, KernelAttribs.insert lr1item attribs kernel_attribs'
          | Some attribs_prev -> begin
              Attribs.fold ~init:(do_insert, kernel_attribs')
                ~f:(fun (do_insert, kernel_attribs')
                  (Attrib.{conflict_state_index; symbol_index; _} as attrib) ->
                  match Attribs.get ~conflict_state_index ~symbol_index attribs_prev with
                  | None ->
                    true, KernelAttribs.insert lr1item (Attribs.singleton attrib) kernel_attribs'
                  | Some attrib_prev -> begin
                      let attrib' = Attrib.diff attrib attrib_prev in
                      match Attrib.is_empty attrib' with
                      | true -> do_insert, kernel_attribs'
                      | false -> begin
                          true,
                          KernelAttribs.insert lr1item (Attribs.singleton attrib') kernel_attribs'
                        end
                    end
                ) attribs
            end
        ) ipred_kernel_attribs in
      (* Avoid recursing if no new transit attribs are inserted, since no additional insertions
       * will occur in the recursion. *)
      match do_insert with
      | false -> lalr1_kernel_attribs
      | true -> begin
          let lalr1_kernel_attribs =
            Ordmap.upsert ~k:transit ~v:kernel_attribs' lalr1_kernel_attribs in
          (* Recurse if lanes may extend to predecessors. *)
          match LaneCtx.traces_length ipred_lanectx with
          | 0L -> lalr1_kernel_attribs
          | _ -> ipred_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs
              ~lalr1_kernel_attribs ipred_lanectx
        end
    ) (Adjs.ipreds_of_state_index state_index adjs)

let gather_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_kernel_attribs
    conflict_state =
  let lanectx = LaneCtx.of_conflict_state ~resolve symbols prods conflict_state in
  ipred_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs ~lalr1_kernel_attribs
    lanectx

let lalr1_kernel_attribs_init ~resolve io symbols prods lalr1_states =
  let adjs = Adjs.init lalr1_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let io, lalr1_kernel_attribs =
    Array.fold ~init:(io, Ordmap.empty (module Transit))
      ~f:(fun (io, lalr1_kernel_attribs) state ->
        match State.has_conflict_attribs ~resolve symbols prods state with
        | false -> io, lalr1_kernel_attribs
        | true -> begin
            let io = io.log |> Fmt.fmt "." |> Io.with_log io in
            let lalr1_kernel_attribs =
              gather_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs
                ~lalr1_kernel_attribs state in
            io, lalr1_kernel_attribs
          end
      ) lalr1_states
  in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  io, lalr1_kernel_attribs

(* Create lookup function for attribs that closes on the prerequisite LALR(1) inadequacy analysis.
*)
let gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states =
  let io, lalr1_kernel_attribs = lalr1_kernel_attribs_init ~resolve io symbols prods lalr1_states in
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
    let kernel_attribs = match Ordmap.get transit lalr1_kernel_attribs with
      | None -> KernelAttribs.empty
      | Some kernel_attribs -> kernel_attribs
    in
    GotoNub.init ~isocores_sn_opt:(Some isocores_sn) ~goto ~kernel_attribs
  end in
  io, gotonub_of_statenub_goto
