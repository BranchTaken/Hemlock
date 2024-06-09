open Basis
open! Basis.Rudiments

let rec pred_annotations ~resolve symbols prods lalr1_states adjs annotations lanectx =
  (* Accumulate kernel attribs of ipred lane contexts. *)
  Array.fold ~init:annotations ~f:(fun annotations ipred_state_index ->
    let ipred_state = Array.get ipred_state_index lalr1_states in
    let ipred_lanectx = LaneCtx.of_ipred ipred_state lanectx in
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
    | false, _ -> annotations
    | true, kernel_attribs' -> begin
        let annotations = Ordmap.upsert ~k:transit ~v:kernel_attribs' annotations in
        (* Recurse if lanes may extend to predecessors. *)
        match LaneCtx.traces_length ipred_lanectx with
        | 0L -> annotations
        | _ -> pred_annotations ~resolve symbols prods lalr1_states adjs annotations
            ipred_lanectx
      end
  ) (Adjs.ipreds_of_state (LaneCtx.state lanectx) adjs)

let attribset_compat ~resolve symbols prods attribset =
  let rec inner ~resolve symbols prods attrib0 attribset_seq_base attribset_seq_cur = begin
    match Ordset.Seq.next_opt attribset_seq_cur with
    | None -> begin
        (* Advance attrib0. *)
        match Ordset.Seq.next_opt attribset_seq_base with
        | None -> true
        | Some (attrib0', attribset_seq_base') ->
          inner ~resolve symbols prods attrib0' attribset_seq_base' attribset_seq_base'
      end
    | Some (attribn, attribset_seq_cur') -> begin
        match Attrib.compat_ielr1 ~resolve symbols prods attrib0 attribn with
        | false -> false
        | true -> inner ~resolve symbols prods attribn attribset_seq_base attribset_seq_cur'
      end
  end in
  match Ordset.length attribset <= 1L with
  | true -> true
  | false -> begin
      let attrib0, attribset_seq = Ordset.Seq.init attribset |> Ordset.Seq.next in
      inner ~resolve symbols prods attrib0 attribset_seq attribset_seq
    end

let filter_useless_annotations ~resolve symbols prods annotations_all =
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
          (* A shift attrib can flow in via an unannotated in-transit to any predecessor, thus
           * making accurate determination of implicit shift attrib presence rather expensive to
           * compute. This implementation takes the simpler conservative approach of assuming that
           * an implicit shift attrib exists if the conflict contains shift. *)
          match Contrib.mem_shift conflict with
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

let gather_transit_kernel_attribs ~resolve symbols prods lalr1_states adjs conflict_state =
  LaneCtx.of_conflict_state ~resolve symbols prods conflict_state
  |> pred_annotations ~resolve symbols prods lalr1_states adjs (Ordmap.empty (module Transit))
  |> filter_useless_annotations ~resolve symbols prods

let annotations_init ~resolve io symbols prods lalr1_states =
  let adjs = Adjs.init lalr1_states in
  (* Gather transit attribs for all conflict states. *)
  let io =
    io.log
    |> Fmt.fmt "hocc: Gathering IELR(1) conflict attributions"
    |> Io.with_log io
  in
  let io, annotations =
    Array.fold ~init:(io, Ordmap.empty (module Transit))
      ~f:(fun (io, annotations) state ->
        match State.has_conflict_attribs ~resolve symbols prods state with
        | false -> io, annotations
        | true -> begin
            let io = io.log |> Fmt.fmt "." |> Io.with_log io in
            let state_annotations = gather_transit_kernel_attribs ~resolve symbols prods
                lalr1_states adjs state in
            let annotations = Ordmap.union ~f:(fun _transit ka0 ka1 -> KernelAttribs.union ka0 ka1)
                state_annotations annotations in
            io, annotations
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
