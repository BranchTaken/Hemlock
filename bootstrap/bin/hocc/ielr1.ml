open Basis
open! Basis.Rudiments

(* Enqueue conflict state's ipred transits in preparation for annotation closure. *)
let enq_ipred_lanectxs lalr1_states adjs leftmost_cache lanectxs_pending workq
    conflict_state_lanectx =
  let conflict_state = LaneCtx.state conflict_state_lanectx in
  let conflict_state_index = State.index conflict_state in
  Array.fold ~init:(leftmost_cache, lanectxs_pending, workq)
    ~f:(fun (leftmost_cache, lanectxs_pending, workq) ipred_state_index ->
      let ipred_transit = Transit.init ~src:ipred_state_index ~dst:conflict_state_index in
      let ipred_state = Array.get ipred_state_index lalr1_states in
      let ipred_lanectx, leftmost_cache =
        LaneCtx.of_ipred_state ipred_state leftmost_cache conflict_state_lanectx in
      let lanectxs_pending = Map.insert_hlt ~k:ipred_transit ~v:ipred_lanectx lanectxs_pending in
      let workq = Workq.push_back ipred_transit workq in
      leftmost_cache, lanectxs_pending, workq
    ) (Adjs.ipreds_of_state (LaneCtx.state conflict_state_lanectx) adjs)

let rec close_lanectxs lalr1_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq =
  (* Filter already-traced lanes, if any. *)
  let filter_traced_ipred_lanectx ipred_lanectx_closed_opt ipred_lanectx = begin
    match ipred_lanectx_closed_opt with
    | None -> ipred_lanectx
    | Some ipred_lanectx_closed -> LaneCtx.diff ipred_lanectx ipred_lanectx_closed
  end in
  match Workq.is_empty workq with
  | true -> leftmost_cache, lanectxs_closed
  | false -> begin
      let transit, workq = Workq.pop workq in
      let lanectx = Map.get_hlt transit lanectxs_pending in
      let lanectxs_pending = Map.remove_hlt transit lanectxs_pending in
      let state = LaneCtx.state lanectx in
      let state_index = State.index state in
      (* Enqueue ipred lane contexts if new transit attribs are inserted... *)
      let leftmost_cache, workq, lanectxs_closed, lanectxs_pending =
        match LaneCtx.is_empty lanectx with
        | true -> leftmost_cache, workq, lanectxs_closed, lanectxs_pending
        | false -> begin
            let lanectxs_closed = Map.amend transit ~f:(fun lanectx_closed_opt ->
              match lanectx_closed_opt with
              | None -> Some lanectx
              | Some lanectx_closed -> Some (LaneCtx.union lanectx lanectx_closed)
            ) lanectxs_closed in
            let leftmost_cache, workq, lanectxs_closed, lanectxs_pending =
              Array.fold ~init:(leftmost_cache, workq, lanectxs_closed, lanectxs_pending)
                ~f:(fun (leftmost_cache, workq, lanectxs_closed, lanectxs_pending)
                  ipred_state_index ->
                  let ipred_transit = Transit.init ~src:ipred_state_index ~dst:state_index in
                  let ipred_lanectx_closed_opt = Map.get ipred_transit lanectxs_closed in
                  let ipred_state = Array.get ipred_state_index lalr1_states in
                  let ipred_lanectx, leftmost_cache =
                    LaneCtx.of_ipred_state ipred_state leftmost_cache lanectx in
                  let leftmost_cache, lanectxs_closed, lanectxs_pending, workq =
                    match Map.get ipred_transit lanectxs_pending with
                    | None -> begin
                        let ipred_lanectx =
                          filter_traced_ipred_lanectx ipred_lanectx_closed_opt ipred_lanectx in
                        (* ... and if previously untraced lanes may extend to predecessors. *)
                        let lanectxs_pending, workq = match LaneCtx.is_empty ipred_lanectx with
                          | true -> lanectxs_pending, workq
                          | false -> begin
                              let lanectxs_pending =
                                Map.insert_hlt ~k:ipred_transit ~v:ipred_lanectx
                                  lanectxs_pending in
                              let workq = Workq.push ipred_transit workq in
                              lanectxs_pending, workq
                            end
                        in
                        leftmost_cache, lanectxs_closed, lanectxs_pending, workq
                      end
                    | Some ipred_lanectx_existing -> begin
                        let ipred_lanectx = LaneCtx.union ipred_lanectx_existing ipred_lanectx in
                        let ipred_lanectx =
                          filter_traced_ipred_lanectx ipred_lanectx_closed_opt ipred_lanectx in
                        (* ... and if previously untraced lanes may extend to predecessors. *)
                        let lanectxs_pending, workq = match LaneCtx.is_empty ipred_lanectx with
                          | true -> lanectxs_pending, workq
                          | false -> begin
                              let lanectxs_pending =
                                Map.update_hlt ~k:ipred_transit ~v:ipred_lanectx
                                  lanectxs_pending in
                              let workq = match Workq.mem ipred_transit workq with
                                | true -> workq
                                | false -> Workq.push_back ipred_transit workq
                              in
                              lanectxs_pending, workq
                            end
                        in
                        leftmost_cache, lanectxs_closed, lanectxs_pending, workq
                      end
                  in
                  leftmost_cache, workq, lanectxs_closed, lanectxs_pending
                ) (Adjs.ipreds_of_state state adjs)
            in
            leftmost_cache, workq, lanectxs_closed, lanectxs_pending
          end
      in
      close_lanectxs lalr1_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq
    end

let has_implicit_shift_attribs adjs annotations ~conflict_state_index ~symbol_index ~conflict dst =
  (* dst has implicit shift-only attribs if the conflict contains shift, and at least one
   * (transitive) in-transit lacks an attrib on symbol_index. *)
  let rec inner adjs annotations ~conflict_state_index ~symbol_index dst present lacking
      marks = begin
    (* There must be at least one explicit attrib for an implicit shift attrib to matter. *)
    let present, lacking, marks = Array.fold_until ~init:(present, lacking, marks)
      ~f:(fun (present, lacking, marks) src ->
        let transit = Transit.init ~src ~dst in
        let present, lacking, marks = match Ordmap.get transit annotations with
          | None -> present, true, marks
          | Some kernel_attribs -> begin
              let ka_present, ka_lacking = KernelAttribs.fold_until ~init:(false, false)
                ~f:(fun (ka_present, ka_lacking) (_kernel_item, attribs) ->
                  let ka_present, ka_lacking =
                    match Attribs.get ~conflict_state_index ~symbol_index attribs with
                    | None -> ka_present, true
                    | Some _attrib -> true, ka_lacking
                  in
                  (ka_present, ka_lacking), ka_present && ka_lacking
                ) kernel_attribs in
              let present, lacking, marks = match ka_present, lacking || ka_lacking with
                | true, true -> true, true, marks
                | true, false -> begin
                    match Set.mem src marks with
                    | true -> true, false, marks
                    | false -> begin
                        inner adjs annotations ~conflict_state_index ~symbol_index src true false
                          (Set.insert src marks)
                      end
                  end
                | false, true -> present, true, marks
                | false, false -> not_reached ()
              in
              present, lacking, marks
            end in
        (present, lacking, marks), present && lacking
      ) (Adjs.ipreds_of_state_index dst adjs) in
    present, lacking, marks
  end in
  match Contrib.mem_shift conflict with
  | false -> false
  | true -> begin
      let present, lacking, _marks = inner adjs annotations ~conflict_state_index ~symbol_index dst
          false false (Set.singleton (module State.Index) dst) in
      let has_implicit_shift = present && lacking in
      has_implicit_shift
    end

let attribset_compat ~resolve symbols prods attribset =
  (* Determine whether all pairs of attribs in attribset are compatible. *)
  let rec inner ~resolve symbols prods attrib0 attribset_seq_base attribset_seq_cur = begin
    match Ordset.Seq.next_opt attribset_seq_cur with
    | None -> begin
        (* Advance attrib0. *)
        match Ordset.Seq.next_opt attribset_seq_base with
        | None -> true
        | Some (attrib0', attribset_seq_base') ->
          inner ~resolve symbols prods attrib0' attribset_seq_base' attribset_seq_base'
      end
    | Some (attrib1, attribset_seq_cur') -> begin
        match Attrib.compat_ielr1 ~resolve symbols prods attrib0 attrib1 with
        | false -> false
        | true -> inner ~resolve symbols prods attrib1 attribset_seq_base attribset_seq_cur'
      end
  end in
  match Ordset.length attribset <= 1L with
  | true -> true
  | false -> begin
      let attrib0, attribset_seq = Ordset.Seq.init attribset |> Ordset.Seq.next in
      inner ~resolve symbols prods attrib0 attribset_seq attribset_seq
    end

let filter_useless_annotations ~resolve symbols prods adjs annotations_all =
  (* Create a {destination state}->{conflict state}->{symbol}->{attrib set} map and use it to
   * distinguish useful vs useless annotations. *)
  let dst_cs_sym_attribsets_shiftless = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
    ~f:(fun dst_cs_sym_attribsets (Transit.{dst; _}, kernel_attribs) ->
      Ordmap.amend dst ~f:(fun cs_sym_attribsets_opt ->
        let cs_sym_attribsets = Option.value cs_sym_attribsets_opt
            ~default:(Ordmap.empty (module State.Index)) in
        let cs_sym_attribsets = KernelAttribs.fold ~init:cs_sym_attribsets
            ~f:(fun cs_sym_attribsets (_kernel_item, attribs) ->
              Attribs.fold ~init:cs_sym_attribsets
                ~f:(fun cs_sym_attribsets
                  Attrib.{conflict_state_index=cs; symbol_index=sym; conflict; contrib; _} ->
                  let attrib = Attrib.init ~conflict_state_index:cs ~symbol_index:sym ~conflict
                      ~isucc_lr1itemset:Lr1Itemset.empty ~contrib in
                  Ordmap.amend cs ~f:(fun sym_attribset_opt ->
                    let sym_attribset = match sym_attribset_opt with
                      | None -> begin
                          let attribset = Ordset.singleton (module Attrib) attrib in
                          Ordmap.singleton (module State.Index) ~k:sym ~v:attribset
                        end
                      | Some sym_attribset -> begin
                          Ordmap.amend sym ~f:(fun attribset_opt ->
                            let attribset = match attribset_opt with
                              | None -> Ordset.singleton (module Attrib) attrib
                              | Some attribset -> Ordset.insert attrib attribset
                            in
                            Some attribset
                          ) sym_attribset
                        end
                    in
                    Some sym_attribset
                  ) cs_sym_attribsets
                ) attribs
            ) kernel_attribs in
        Some cs_sym_attribsets
      ) dst_cs_sym_attribsets
    ) annotations_all in
  (* Integrate any implicit shift attribs. *)
  let dst_cs_sym_attribsets = Ordmap.fold ~init:dst_cs_sym_attribsets_shiftless
      ~f:(fun dst_cs_sym_attribsets (dst, cs_sym_attribsets) ->
        let cs_sym_attribsets, integrated_cs_sym_attribsets =
          Ordmap.fold ~init:(cs_sym_attribsets, false)
            ~f:(fun (cs_sym_attribsets, integrated_cs_sym_attribsets) (cs, sym_attribsets) ->
              let sym_attribsets, integrated_sym_attribsets =
                Ordmap.fold ~init:(sym_attribsets, false)
                  ~f:(fun (sym_attribsets, integrated_sym_attribsets) (sym, attribset) ->
                    let Attrib.{conflict_state_index; symbol_index; conflict; _} =
                      Ordset.choose_hlt attribset in
                    assert State.Index.(conflict_state_index = cs);
                    assert Symbol.Index.(symbol_index = sym);
                    let attribset, integrated_attribset =
                      match has_implicit_shift_attribs adjs annotations_all ~conflict_state_index
                          ~symbol_index ~conflict dst with
                      | false -> attribset, false
                      | true -> begin
                          let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
                              ~isucc_lr1itemset:Lr1Itemset.empty ~contrib:Contrib.shift in
                          let attribset = Ordset.insert attrib attribset in
                          attribset, true
                        end
                    in
                    match integrated_attribset with
                    | false -> sym_attribsets, integrated_sym_attribsets
                    | true -> Ordmap.update_hlt ~k:sym ~v:attribset sym_attribsets, true
                  ) sym_attribsets in
              match integrated_sym_attribsets with
              | false -> cs_sym_attribsets, integrated_cs_sym_attribsets
              | true -> Ordmap.update_hlt ~k:cs ~v:sym_attribsets cs_sym_attribsets, true
            ) cs_sym_attribsets in
        match integrated_cs_sym_attribsets with
        | false -> dst_cs_sym_attribsets
        | true -> Ordmap.update_hlt ~k:dst ~v:cs_sym_attribsets dst_cs_sym_attribsets
      ) dst_cs_sym_attribsets_shiftless in
  (* Per conflict state annotations regarding symbols for which any attribs are incompatible are
   * useful; all other annotations are useless. *)
  let dst_cs_syms_useful = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
    ~f:(fun dst_cs_syms_useful (dst, cs_sym_attribsets) ->
      let cs_syms_useful = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
        ~f:(fun cs_syms_useful (cs, sym_attribsets) ->
          let syms_useful = Ordmap.fold ~init:Bitset.empty
              ~f:(fun syms_useful (sym, attribset) ->
                match attribset_compat ~resolve symbols prods attribset with
                | true -> syms_useful
                | false -> Bitset.insert sym syms_useful
              ) sym_attribsets in
          match Bitset.is_empty syms_useful with
          | true -> cs_syms_useful
          | false -> Ordmap.insert_hlt ~k:cs ~v:syms_useful cs_syms_useful
        ) cs_sym_attribsets in
      match Ordmap.is_empty cs_syms_useful with
      | true -> dst_cs_syms_useful
      | false -> Ordmap.insert_hlt ~k:dst ~v:cs_syms_useful dst_cs_syms_useful
    ) dst_cs_sym_attribsets in
  (* Finally, filter useless annotations. *)
  Ordmap.fold ~init:(Ordmap.empty (module Transit))
    ~f:(fun annotations_useful ((Transit.{dst; _} as transit), kernel_attribs) ->
      match Ordmap.get dst dst_cs_syms_useful with
      | None -> annotations_useful
      | Some cs_syms_useful -> begin
          let kernel_attribs = KernelAttribs.fold ~init:KernelAttribs.empty
              ~f:(fun kernel_attribs (kernel_item, attribs) ->
                let attribs = Attribs.fold ~init:Attribs.empty
                    ~f:(fun attribs
                      (Attrib.{conflict_state_index=cs; symbol_index=sym; _} as attrib) ->
                      match Ordmap.get cs cs_syms_useful with
                      | None -> attribs
                      | Some syms_useful -> begin
                          match Bitset.mem sym syms_useful with
                          | false -> attribs
                          | true -> Attribs.insert attrib attribs
                        end
                    ) attribs in
                match Attribs.is_empty attribs with
                | true -> kernel_attribs
                | false -> KernelAttribs.insert kernel_item attribs kernel_attribs
              ) kernel_attribs in
          match KernelAttribs.is_empty kernel_attribs with
          | true -> annotations_useful
          | false -> Ordmap.insert ~k:transit ~v:kernel_attribs annotations_useful
        end
    ) annotations_all

let annotations_init ~resolve io symbols prods lalr1_states =
  let adjs = Adjs.init lalr1_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let leftmost_cache = Lr1ItemsetClosure.LeftmostCache.empty in
  let lanectxs_closed = Map.empty (module Transit) in
  let lanectxs_pending = Map.empty (module Transit) in
  let workq = Workq.empty (module Transit) in
  let leftmost_cache, lanectxs_closed, lanectxs_pending, workq =
    Array.fold ~init:(leftmost_cache, lanectxs_closed, lanectxs_pending, workq)
      ~f:(fun (leftmost_cache, lanectxs_closed, lanectxs_pending, workq) conflict_state ->
        match State.has_conflict_attribs ~resolve symbols prods conflict_state with
        | false -> leftmost_cache, lanectxs_closed, lanectxs_pending, workq
        | true -> begin
            let conflict_lanectx, leftmost_cache =
              LaneCtx.of_conflict_state ~resolve symbols prods leftmost_cache conflict_state in
            let leftmost_cache, lanectxs_pending, workq =
              enq_ipred_lanectxs lalr1_states adjs leftmost_cache lanectxs_pending workq
                conflict_lanectx
            in
            leftmost_cache, lanectxs_closed, lanectxs_pending, workq
          end
      ) lalr1_states
  in
  let _leftmost_cache, lanectxs_closed =
    close_lanectxs lalr1_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq in
  let io =
    io.log
    |> Fmt.fmt "\n"
    |> Fmt.fmt "hocc: Filtering useless IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let annotations = Map.fold ~init:(Ordmap.empty (module Transit))
    ~f:(fun annotations (transit, lanectx) ->
      let kernel_attribs = LaneCtx.kernel_attribs lanectx in
      match KernelAttribs.is_empty kernel_attribs with
      | true -> annotations
      | false -> Ordmap.insert_hlt ~k:transit ~v:kernel_attribs annotations
    ) lanectxs_closed in
  let annotations = filter_useless_annotations ~resolve symbols prods adjs annotations in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  io, annotations

(* Create lookup function for attribs that closes on the prerequisite LALR(1) inadequacy analysis.
*)
let gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states =
  let io, annotations = annotations_init ~resolve io symbols prods lalr1_states in
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
    let kernel_attribs = match Ordmap.get transit annotations with
      | None -> KernelAttribs.empty
      | Some kernel_attribs -> kernel_attribs
    in
    GotoNub.init ~isocores_sn_opt:(Some isocores_sn) ~goto ~kernel_attribs
  end in
  io, gotonub_of_statenub_goto
