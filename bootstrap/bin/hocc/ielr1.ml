open Basis
open! Basis.Rudiments

let rec pred_annotations ~resolve symbols prods lalr1_states adjs leftmost_cache annotations
    lanectx =
  (* Accumulate kernel attribs of ipred lane contexts. *)
  Array.fold ~init:(leftmost_cache, annotations)
    ~f:(fun (leftmost_cache, annotations) ipred_state_index ->
      let ipred_state = Array.get ipred_state_index lalr1_states in
      let ipred_lanectx, leftmost_cache = LaneCtx.of_ipred ipred_state leftmost_cache lanectx in
      let ipred_kernel_attribs = LaneCtx.kernel_attribs ipred_lanectx in
      let transit = LaneCtx.transit ipred_lanectx in
      (* Load any existing kernel attribs, whether to other conflict states, or as a result of
       * recursing into a lane cycle. *)
      let kernel_attribs =
        Ordmap.get transit annotations
        |> Option.value ~default:KernelAttribs.empty
      in
      (* Avoid recursing if no new transit attribs are inserted, since no additional insertions will
       * occur in the recursion. *)
      match KernelAttribs.merge ipred_kernel_attribs kernel_attribs with
      | false, _ -> leftmost_cache, annotations
      | true, kernel_attribs' -> begin
          let annotations = Ordmap.upsert ~k:transit ~v:kernel_attribs' annotations in
          (* Recurse if lanes may extend to predecessors. *)
          match LaneCtx.traces_length ipred_lanectx with
          | 0L -> leftmost_cache, annotations
          | _ -> pred_annotations ~resolve symbols prods lalr1_states adjs leftmost_cache
              annotations ipred_lanectx
        end
    ) (Adjs.ipreds_of_state (LaneCtx.state lanectx) adjs)

let has_implicit_shift_attribs adjs annotations ~conflict_state_index ~symbol_index ~conflict dst =
  (* dst has implicit shift-only attribs if the conflict contains shift, and at least one
   * (transitive) in-transit lacks an attrib on symbol_index. *)
  let rec inner adjs annotations ~conflict_state_index ~symbol_index ~conflict marks dst = begin
    (* There must be at least one explicit attrib for an implicit shift attrib to matter. *)
    let present, lacking, marks = Array.fold_until ~init:(false, false, marks)
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
                    let ka_lacking, marks = match Ordset.mem src marks with
                      | true -> false, marks
                      | false -> begin
                          let has_implicit_shift, marks = inner adjs annotations
                              ~conflict_state_index ~symbol_index ~conflict
                              (Ordset.insert src marks) src in
                          has_implicit_shift, marks
                        end
                    in
                    true, ka_lacking, marks
                  end
                | false, true -> present, true, marks
                | false, false -> not_reached ()
              in
              present, lacking, marks
            end in
        (present, lacking, marks), present && lacking
      ) (Adjs.ipreds_of_state_index dst adjs) in
    let has_implicit_shift = present && lacking in
    has_implicit_shift, marks
  end in
  match Contrib.mem_shift conflict with
  | false -> false
  | true -> begin
      let has_implicit_shift, _marks = inner adjs annotations ~conflict_state_index ~symbol_index
          ~conflict (Ordset.singleton (module State.Index) dst) dst in
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
  (* Create a per destination state map of per symbol attrib sets and use it to distinguish useful
   * vs useless annotations. *)
  let dst_sym_attribsets_shiftless = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
    ~f:(fun dst_sym_attribsets (Transit.{dst; _}, kernel_attribs) ->
      Ordmap.amend dst ~f:(fun sym_attribset_opt ->
        let sym_attribset = Option.value sym_attribset_opt
            ~default:(Ordmap.empty (module Symbol.Index)) in
        let sym_attribset' = KernelAttribs.fold ~init:sym_attribset
            ~f:(fun sym_attribset' (_kernel_item, attribs) ->
              Attribs.fold ~init:sym_attribset'
                ~f:(fun sym_attribset'
                  Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
                  let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
                      ~isucc_lr1itemset:Lr1Itemset.empty ~contrib in
                  Ordmap.amend symbol_index ~f:(fun attribset_opt ->
                    let attribset' = match attribset_opt with
                      | None -> Ordset.singleton (module Attrib) attrib
                      | Some attribset -> Ordset.insert attrib attribset
                    in
                    Some attribset'
                  ) sym_attribset'
                ) attribs
            ) kernel_attribs in
        Some sym_attribset'
      ) dst_sym_attribsets
    ) annotations_all in
  (* Integrate any implicit shift attribs. *)
  let dst_sym_attribsets = Ordmap.fold ~init:dst_sym_attribsets_shiftless
      ~f:(fun dst_sym_attribsets (dst, sym_attribsets) ->
        let sym_attribsets' = Ordmap.fold ~init:sym_attribsets
            ~f:(fun sym_attribsets' (sym, attribset) ->
              let Attrib.{conflict_state_index; symbol_index; conflict; _} =
                Ordset.choose_hlt attribset in
              assert Symbol.Index.(symbol_index = sym);
              match has_implicit_shift_attribs adjs annotations_all ~conflict_state_index
                  ~symbol_index ~conflict dst with
              | false -> sym_attribsets'
              | true -> begin
                  let attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
                      ~isucc_lr1itemset:Lr1Itemset.empty ~contrib:Contrib.shift in
                  let attribset' = Ordset.insert attrib attribset in
                  Ordmap.update_hlt ~k:sym ~v:attribset' sym_attribsets'
                end
            ) sym_attribsets in
        Ordmap.update_hlt ~k:dst ~v:sym_attribsets' dst_sym_attribsets
      ) dst_sym_attribsets_shiftless in
  (* Annotations regarding symbols for which any attribs are incompatible are useful; all other
   * annotations are useless. *)
  let dst_syms_useful = Ordmap.fold ~init:(Ordmap.empty (module State.Index))
    ~f:(fun dst_syms_useful (dst, sym_attribsets) ->
      Ordmap.fold ~init:dst_syms_useful
        ~f:(fun dst_syms_useful (sym, attribset) ->
          match attribset_compat ~resolve symbols prods attribset with
          | true -> dst_syms_useful
          | false -> begin
              Ordmap.amend dst ~f:(fun syms_useful_opt ->
                let syms_useful' = match syms_useful_opt with
                  | None -> Ordset.singleton (module Symbol.Index) sym
                  | Some syms_useful -> Ordset.insert sym syms_useful
                in
                Some syms_useful'
              ) dst_syms_useful
            end
        ) sym_attribsets
    ) dst_sym_attribsets in
  Ordmap.fold ~init:(Ordmap.empty (module Transit))
    ~f:(fun annotations_useful ((Transit.{dst; _} as transit), kernel_attribs) ->
      match Ordmap.get dst dst_syms_useful with
      | None -> annotations_useful
      | Some syms_useful -> begin
          let kernel_attribs' = KernelAttribs.fold ~init:KernelAttribs.empty
              ~f:(fun kernel_attribs' ((Lr1Item.{follow; _} as kernel_item), attribs) ->
                assert (Ordset.length follow = 1L);
                let sym = Ordset.choose_hlt follow in
                match Ordset.mem sym syms_useful with
                | false -> kernel_attribs'
                | true ->  KernelAttribs.insert kernel_item attribs kernel_attribs'
              ) kernel_attribs in
          match KernelAttribs.is_empty kernel_attribs' with
          | true -> annotations_useful
          | false -> Ordmap.insert ~k:transit ~v:kernel_attribs' annotations_useful
        end
    ) annotations_all

let gather_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs conflict_state
    leftmost_cache =
  let lanectx, leftmost_cache = LaneCtx.of_conflict_state ~resolve symbols prods leftmost_cache
      conflict_state in
  let leftmost_cache, annotations = pred_annotations ~resolve symbols prods lalr1_states adjs
      leftmost_cache (Ordmap.empty (module Transit)) lanectx in
  let annotations = filter_useless_annotations ~resolve symbols prods adjs annotations in
  leftmost_cache, annotations

let annotations_init ~resolve io symbols prods lalr1_states =
  let adjs = Adjs.init lalr1_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let io, _leftmost_cache, annotations =
    Array.fold ~init:(io, Lr1ItemsetClosure.LeftmostCache.empty, Ordmap.empty (module Transit))
      ~f:(fun (io, leftmost_cache, annotations) state ->
        match State.has_conflict_attribs ~resolve symbols prods state with
        | false -> io, leftmost_cache, annotations
        | true -> begin
            let io = io.log |> Fmt.fmt "." |> Io.with_log io in
            let leftmost_cache, state_annotations = gather_transit_kernel_attribs ~resolve symbols
                prods lalr1_states adjs state leftmost_cache in
            let annotations = Ordmap.union ~f:(fun _transit ka0 ka1 -> KernelAttribs.union ka0 ka1)
              state_annotations annotations in
            io, leftmost_cache, annotations
          end
      ) lalr1_states
  in
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
