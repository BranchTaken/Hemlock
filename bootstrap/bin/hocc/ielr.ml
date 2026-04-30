open Basis
open! Basis.Rudiments

(* Enqueue conflict state's ipred transits in preparation for annotation closure. *)
let enq_ipred_lanectxs lalr_states adjs leftmost_cache conflict_state_lanectx =
  let lanectxs_pending = Ordmap.empty (module Transit) in
  let workq = Workq.empty (module Transit) in
  let conflict_state = LaneCtx.state conflict_state_lanectx in
  let conflict_state_index = State.index conflict_state in
  Array.fold ~init:(leftmost_cache, lanectxs_pending, workq)
    ~f:(fun (leftmost_cache, lanectxs_pending, workq) ipred_state_index ->
      let ipred_transit = Transit.init ~src:ipred_state_index ~dst:conflict_state_index in
      let ipred_state = Array.get ipred_state_index lalr_states in
      let ipred_lanectx, leftmost_cache =
        LaneCtx.of_ipred_state ipred_state leftmost_cache conflict_state_lanectx in
      let lanectxs_pending = Ordmap.insert_hlt ~k:ipred_transit ~v:ipred_lanectx lanectxs_pending in
      let workq = Workq.push_back ipred_transit workq in
      leftmost_cache, lanectxs_pending, workq
    ) (Adjs.ipreds_of_state (LaneCtx.state conflict_state_lanectx) adjs)

let rec close_lanectxs_impl lalr_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq =
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
      let lanectx = Ordmap.get_hlt transit lanectxs_pending in
      let lanectxs_pending = Ordmap.remove_hlt transit lanectxs_pending in
      let state = LaneCtx.state lanectx in
      let state_index = State.index state in
      (* Enqueue ipred lane contexts if new transit attribs are inserted... *)
      let leftmost_cache, workq, lanectxs_closed, lanectxs_pending =
        match LaneCtx.is_empty lanectx with
        | true -> leftmost_cache, workq, lanectxs_closed, lanectxs_pending
        | false -> begin
            let did_merge, lanectxs_closed = match Ordmap.get transit lanectxs_closed with
              | None -> true, Ordmap.insert_hlt ~k:transit ~v:lanectx lanectxs_closed
              | Some lanectx_closed -> begin
                  let did_merge, lanectx_closed = LaneCtx.merge lanectx lanectx_closed in
                  let lanectxs_closed = match did_merge with
                    | false -> lanectxs_closed
                    | true -> Ordmap.update_hlt ~k:transit ~v:lanectx_closed lanectxs_closed
                  in
                  did_merge, lanectxs_closed
                end
            in
            (* ... and if lanes are merged into `lanectxs_closed` ... *)
            match did_merge with
            | false -> leftmost_cache, workq, lanectxs_closed, lanectxs_pending
            | true -> begin
                let leftmost_cache, workq, lanectxs_closed, lanectxs_pending =
                  Array.fold ~init:(leftmost_cache, workq, lanectxs_closed, lanectxs_pending)
                    ~f:(fun (leftmost_cache, workq, lanectxs_closed, lanectxs_pending)
                      ipred_state_index ->
                      let ipred_transit = Transit.init ~src:ipred_state_index ~dst:state_index in
                      let ipred_lanectx_closed_opt = Ordmap.get ipred_transit lanectxs_closed in
                      let ipred_state = Array.get ipred_state_index lalr_states in
                      let ipred_lanectx, leftmost_cache =
                        LaneCtx.of_ipred_state ipred_state leftmost_cache lanectx in
                      let leftmost_cache, lanectxs_closed, lanectxs_pending, workq =
                        match Ordmap.get ipred_transit lanectxs_pending with
                        | None -> begin
                            let ipred_lanectx =
                              filter_traced_ipred_lanectx ipred_lanectx_closed_opt ipred_lanectx in
                            (* ... and if previously untraced lanes may extend to predecessors. *)
                            let lanectxs_pending, workq = match LaneCtx.is_empty ipred_lanectx with
                              | true -> lanectxs_pending, workq
                              | false -> begin
                                  let lanectxs_pending =
                                    Ordmap.insert_hlt ~k:ipred_transit ~v:ipred_lanectx
                                      lanectxs_pending in
                                  let workq = Workq.push_back ipred_transit workq in
                                  lanectxs_pending, workq
                                end
                            in
                            leftmost_cache, lanectxs_closed, lanectxs_pending, workq
                          end
                        | Some ipred_lanectx_existing -> begin
                            let ipred_lanectx = ipred_lanectx
                              |> LaneCtx.union ipred_lanectx_existing
                              |> filter_traced_ipred_lanectx ipred_lanectx_closed_opt in
                            (* ... and if previously untraced lanes may extend to predecessors. *)
                            let lanectxs_pending, workq = match LaneCtx.is_empty ipred_lanectx with
                              | true -> lanectxs_pending, workq
                              | false -> begin
                                  let lanectxs_pending =
                                    Ordmap.update_hlt ~k:ipred_transit ~v:ipred_lanectx
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
          end
      in
      close_lanectxs_impl lalr_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq
    end

let close_lanectxs lalr_states adjs leftmost_cache lanectxs_pending workq =
  let lanectxs_closed = Ordmap.empty (module Transit) in
  close_lanectxs_impl lalr_states adjs leftmost_cache lanectxs_closed lanectxs_pending workq

(* {destination state, conflict state, symbol} key, used by `has_isolated_shift_attribs` and
 * `filter_useless_annotations`. *)
module DstCsSym = struct
  module T = struct
    type t = {
      dst: State.Index.t;
      conflict_state_index: State.Index.t;
      symbol_index: Symbol.Index.t;
    }

    let hash_fold {dst; conflict_state_index; symbol_index} state =
      state
      |> State.Index.hash_fold dst
      |> State.Index.hash_fold conflict_state_index
      |> Symbol.Index.hash_fold symbol_index

    let cmp {dst=d0; conflict_state_index=csi0; symbol_index=si0}
      {dst=d1; conflict_state_index=csi1; symbol_index=si1} =
      let open Cmp in
      match State.Index.cmp d0 d1 with
      | Lt -> Lt
      | Eq -> begin
          match Symbol.Index.cmp si0 si1 with
          | Lt -> Lt
          | Eq -> State.Index.cmp csi0 csi1
          | Gt -> Gt
        end
      | Gt -> Gt

    let pp {dst; conflict_state_index; symbol_index} formatter =
      formatter
      |> Fmt.fmt "{dst=" |> State.Index.pp dst
      |> Fmt.fmt "; conflict_state_index=" |> State.Index.pp conflict_state_index
      |> Fmt.fmt "; symbol_index=" |> Symbol.Index.pp symbol_index
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)

  let init ~dst ~conflict_state_index ~symbol_index =
    {dst; conflict_state_index; symbol_index}
end

let has_isolated_shift_attribs adjs annotations isolateds ~dst ~conflict_state_index ~symbol_index
    ~conflict =
  (* dst has isolated (shift-only) attribs if the conflict contains shift, and at least one
   * [transitive] in-transit lacks an attrib on symbol_index. *)
  let rec inner adjs annotations ~dst ~conflict_state_index ~symbol_index isolateds marks = begin
    let ipreds = Adjs.ipreds_of_state_index dst adjs in
    let present, lacking = Array.fold_until ~init:(false, false)
      ~f:(fun (present, lacking) src ->
        let transit = Transit.init ~src ~dst in
        let present, lacking = match Ordmap.get transit annotations with
          | None -> present, true
          | Some kernel_attribs -> begin
              let ka_present = KernelAttribs.fold_until ~init:false
                  ~f:(fun _ka_present (_kernel_lr0item, attribs) ->
                    let ka_present =
                      Attribs.get ~conflict_state_index ~symbol_index attribs
                      |> Option.is_some
                    in
                    ka_present, ka_present
                  ) kernel_attribs in
              let present = present || ka_present in
              let lacking = lacking || (not ka_present) in
              present, lacking
            end in
        (present, lacking), present && lacking
      ) ipreds in
    (* There must be at least one explicit attrib present for an isolated shift attrib to matter. *)
    let marks, has_isolated_shift = match present, lacking with
      | false, _ -> marks, false
      | true, true -> marks, true
      | true, false -> begin
          let marks, has_isolated_shift = Array.fold_until ~init:(marks, false)
            ~f:(fun (marks, _has_isolated_shift) dst ->
              let marks, has_isolated_shift =
                match Ordmap.get (DstCsSym.init ~dst ~conflict_state_index ~symbol_index) isolateds
                with
                | Some has_isolated_shift -> marks, has_isolated_shift
                | None -> begin
                    match Set.mem dst marks with
                    | true -> marks, false
                    | false -> begin
                        inner adjs annotations ~dst ~conflict_state_index ~symbol_index isolateds
                          (Set.insert dst marks)
                      end
                  end
              in
              (marks, has_isolated_shift), has_isolated_shift
            ) ipreds in
          marks, has_isolated_shift
        end
    in
    marks, has_isolated_shift
  end in
  match Contrib.mem_shift conflict with
  | false -> isolateds, false
  | true -> begin
      match Ordmap.get (DstCsSym.init ~dst ~conflict_state_index ~symbol_index) isolateds with
      | Some has_isolated_shift -> isolateds, has_isolated_shift
      | None -> begin
          let _marks, has_isolated_shift = inner adjs annotations ~dst ~conflict_state_index
              ~symbol_index isolateds (Set.singleton (module State.Index) dst)
          in
          let isolateds =
            Ordmap.insert_hlt ~k:(DstCsSym.init ~dst ~conflict_state_index ~symbol_index)
              ~v:has_isolated_shift isolateds in
          isolateds, has_isolated_shift
        end
    end

let attribset_compat ~resolve precs symbols prods attribset =
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
        match Attrib.compat_ielr ~resolve precs symbols prods attrib0 attrib1 with
        | false -> false
        | true -> inner ~resolve symbols prods attrib0 attribset_seq_base attribset_seq_cur'
      end
  end in
  match Ordset.length attribset <= 1L with
  | true -> true
  | false -> begin
      let attrib0, attribset_seq = Ordset.Seq.init attribset |> Ordset.Seq.next in
      inner ~resolve symbols prods attrib0 attribset_seq attribset_seq
    end

let filter_useless_annotations ~resolve precs symbols prods adjs annotations_all =
  (* Create a {destination state, conflict state, symbol}->{attrib set} map and use it to
   * distinguish useful vs useless annotations. *)
  let dst_cs_sym_attribsets_shiftless = Ordmap.fold ~init:(Ordmap.empty (module DstCsSym))
    ~f:(fun dst_cs_sym_attribsets (Transit.{dst; _}, kernel_attribs) ->
      KernelAttribs.fold ~init:dst_cs_sym_attribsets
        ~f:(fun dst_cs_sym_attribsets (_kernel_lr0item, attribs) ->
          Attribs.fold ~init:dst_cs_sym_attribsets
            ~f:(fun dst_cs_sym_attribsets
              Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
              let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
                  ~isucc_lr1itemset:Lr1Itemset.empty ~contrib in
              Ordmap.amend (DstCsSym.init ~dst ~conflict_state_index ~symbol_index)
                ~f:(fun attribset_opt ->
                  let attribset = match attribset_opt with
                    | None -> Ordset.singleton (module Attrib) attrib
                    | Some attribset -> Ordset.insert attrib attribset
                  in
                  Some attribset
                ) dst_cs_sym_attribsets
            ) attribs
        ) kernel_attribs
    ) annotations_all in
  (* Integrate any isolated shift attribs. *)
  let dst_cs_sym_attribsets, _isolateds =
    Ordmap.fold ~init:(dst_cs_sym_attribsets_shiftless, Ordmap.empty (module DstCsSym))
      ~f:(fun (dst_cs_sym_attribsets, isolateds)
        (DstCsSym.{dst; conflict_state_index=cs; symbol_index=sym} as dst_cs_sym, attribset) ->
        let Attrib.{conflict_state_index; symbol_index; conflict; _} =
          Ordset.choose_hlt attribset in
        assert State.Index.(conflict_state_index = cs);
        assert Symbol.Index.(symbol_index = sym);
        match has_isolated_shift_attribs adjs annotations_all isolateds ~dst ~conflict_state_index
            ~symbol_index ~conflict with
        | isolateds, false -> dst_cs_sym_attribsets, isolateds
        | isolateds, true -> begin
            let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
                ~isucc_lr1itemset:Lr1Itemset.empty ~contrib:Contrib.shift in
            let attribset = Ordset.insert attrib attribset in
            Ordmap.update_hlt ~k:dst_cs_sym ~v:attribset dst_cs_sym_attribsets, isolateds
          end
      ) dst_cs_sym_attribsets_shiftless in
  (* Per conflict state annotations regarding symbols for which any attribs are incompatible are
   * useful; all other annotations are useless. *)
  let dst_cs_sym_useful = Ordmap.fold ~init:(Set.empty (module DstCsSym))
    ~f:(fun dst_cs_sym_useful (dst_cs_sym, attribset) ->
      match attribset_compat ~resolve precs symbols prods attribset with
      | true -> dst_cs_sym_useful
      | false -> Set.insert dst_cs_sym dst_cs_sym_useful
    ) dst_cs_sym_attribsets in
  (* Finally, filter useless annotations. *)
  Ordmap.fold ~init:(Ordmap.empty (module Transit))
    ~f:(fun annotations_useful ((Transit.{dst; _} as transit), kernel_attribs) ->
      let kernel_attribs = KernelAttribs.fold ~init:KernelAttribs.empty
          ~f:(fun kernel_attribs (kernel_lr0item, attribs) ->
            let attribs = Attribs.fold ~init:Attribs.empty
                ~f:(fun attribs
                  (Attrib.{conflict_state_index; symbol_index; _} as attrib) ->
                  let dst_cs_sym = DstCsSym.init ~dst ~conflict_state_index ~symbol_index in
                  match Set.mem dst_cs_sym dst_cs_sym_useful with
                  | false -> attribs
                  | true -> Attribs.insert attrib attribs
                ) attribs in
            match Attribs.is_empty attribs with
            | true -> kernel_attribs
            | false -> KernelAttribs.insert kernel_lr0item attribs kernel_attribs
          ) kernel_attribs in
      match KernelAttribs.is_empty kernel_attribs with
      | true -> annotations_useful
      | false -> Ordmap.insert_hlt ~k:transit ~v:kernel_attribs annotations_useful
    ) annotations_all

let annotations_init ~resolve io precs symbols prods lalr_states =
  let adjs = Adjs.init lalr_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict state attributions (/=none/some)"
    |> Io.with_log io
  in
  let leftmost_cache = Lr1ItemsetClosure.LeftmostCache.empty in
  let annotations = Ordmap.empty (module Transit) in
  let _leftmost_cache, io, annotations =
    Array.fold ~init:(leftmost_cache, io, annotations)
      ~f:(fun (leftmost_cache, io, annotations) conflict_state ->
        match State.conflicts ~filter_pseudo_end:false conflict_state,
          State.has_conflict_attribs ~resolve precs symbols prods conflict_state with
        | 0L, false -> leftmost_cache, io, annotations
        | _, false -> begin
            let io = io.log |> Fmt.fmt "" |> Io.with_log io in
            leftmost_cache, io, annotations
          end
        | _, true -> begin
            let io = io.log |> Fmt.fmt "" |> Io.with_log io in
            let conflict_lanectx, leftmost_cache =
              LaneCtx.of_conflict_state ~resolve precs symbols prods leftmost_cache conflict_state
            in
            let leftmost_cache, lanectxs_pending, workq =
              enq_ipred_lanectxs lalr_states adjs leftmost_cache conflict_lanectx in
            let leftmost_cache, lanectxs_closed =
              close_lanectxs lalr_states adjs leftmost_cache lanectxs_pending workq in
            let annotations =
              Ordmap.fold ~init:(Ordmap.empty (module Transit))
                ~f:(fun unfiltered_annotations (transit, lanectx) ->
                  let kernel_attribs = LaneCtx.kernel_attribs lanectx in
                  match KernelAttribs.is_empty kernel_attribs with
                  | true -> unfiltered_annotations
                  | false -> Ordmap.insert_hlt ~k:transit ~v:kernel_attribs unfiltered_annotations
                ) lanectxs_closed
              |> filter_useless_annotations ~resolve precs symbols prods adjs
              |> Ordmap.union ~vunion:(fun _transit ka0 ka1 -> KernelAttribs.union ka0 ka1)
                annotations
            in
            leftmost_cache, io, annotations
          end
      ) lalr_states
  in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  io, annotations

(* Create lookup function for attribs that closes on the prerequisite LALR(1) inadequacy analysis.
*)
let gen_gotonub_of_statenub_goto ~resolve io precs symbols prods lalr_isocores lalr_states =
  let io, annotations = annotations_init ~resolve io precs symbols prods lalr_states in
  let transit_of_statenub_goto statenub goto = begin
    let statenub_core = (Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure.kernel)) in
    let goto_core = Lr1Itemset.core goto in
    let src = Isocores.get_core_hlt statenub_core lalr_isocores |> StateNub.index in
    let dst = Isocores.get_core_hlt goto_core lalr_isocores |> StateNub.index in
    Transit.init ~src ~dst
  end in
  let isocores_sn_of_transit Transit.{dst; _} =
    Isocores.statenub dst lalr_isocores
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
