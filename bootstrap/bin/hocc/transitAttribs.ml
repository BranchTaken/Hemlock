open Basis
open! Basis.Rudiments

type t = {
  (* Union of conflict attributions in `kernel_attribs_all` and and any conflict attributions
   * introduced via `{of_attribs,merge}_{potential,defininte}`. *)
  all: Attribs.t;

  (* Definite conflict attributions, whether shift (conflict state only) or reduce. *)
  definite: Attribs.t;

  (* Per kernel item reduce conflict attributions. Shift attributions are omitted since it is
   * irrelevant which kernel item has a shift attribution, whether definite or potential. *)
  kernel_attribs_all: KernelAttribs.t;
}

let fmt_hr symbols prods ?(alt=false) ?(width=0L) {definite; all; kernel_attribs_all} formatter =
  let lsep = match alt with
    | false -> ""
    | true ->
      String.Fmt.empty
      |> Fmt.fmt "\n"
      |> String.fmt ~pad:(Codepoint.of_char ' ') ~width:(width + 4L) ""
      |> Fmt.to_string
  in
  let sep = match alt with
    | false -> "; "
    | true -> lsep
  in
  let rsep = match alt with
    | false -> ""
    | true ->
      String.Fmt.empty
      |> Fmt.fmt "\n"
      |> String.fmt ~pad:(Codepoint.of_char ' ') ~width:(width + 2L) ""
      |> Fmt.to_string
  in
  formatter
  |> Fmt.fmt "{"
  |> Fmt.fmt sep |> Fmt.fmt "all=" |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) all
  |> Fmt.fmt sep |> Fmt.fmt "definite="
  |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) definite
  |> Fmt.fmt sep |> Fmt.fmt "kernel_attribs_all="
  |> KernelAttribs.fmt_hr symbols prods ~alt ~width:(width + 4L) kernel_attribs_all
  |> Fmt.fmt rsep
  |> Fmt.fmt "}"

let pp {definite; all; kernel_attribs_all} formatter =
  formatter
  |> Fmt.fmt "{all=" |> Attribs.pp all
  |> Fmt.fmt "; definite=" |> Attribs.pp definite
  |> Fmt.fmt "; kernel_attribs_all=" |> KernelAttribs.pp kernel_attribs_all
  |> Fmt.fmt "}"

let empty = {
  all=Attribs.empty;
  definite=Attribs.empty;
  kernel_attribs_all=KernelAttribs.empty
}

let is_empty {all; definite; kernel_attribs_all} =
  Attribs.is_empty all && Attribs.is_empty definite && KernelAttribs.is_empty kernel_attribs_all

let reindex index_map {all; definite; kernel_attribs_all} =
  {
    all=Attribs.reindex index_map all;
    definite=Attribs.reindex index_map definite;
    kernel_attribs_all=KernelAttribs.reindex index_map kernel_attribs_all
  }

let all {all; _} =
  all

let definite {definite; _} =
  definite

let kernel_attribs_all {kernel_attribs_all; _} =
  kernel_attribs_all

let merge_potential lane_attrib ({all; _} as t) =
  assert (Attrib.is_lane_attrib lane_attrib);
  let all = Attribs.insert lane_attrib all in
  {t with all}

let of_attribs_potential lane_attribs =
  {all=lane_attribs; definite=Attribs.empty; kernel_attribs_all=KernelAttribs.empty}

let merge_definite lane_attrib ({definite; _} as t) =
  assert (Attrib.is_lane_attrib lane_attrib);
  let t = merge_potential lane_attrib t in
  let definite = Attribs.insert lane_attrib definite in
  {t with definite}

let of_attribs_definite lane_attribs =
  {all=lane_attribs; definite=lane_attribs; kernel_attribs_all=KernelAttribs.empty}

let insert_kernel_attribs_all kernel_attribs_all t =
  KernelAttribs.fold ~init:t
    ~f:(fun ({kernel_attribs_all; _} as t) (item, attribs) ->
      let t = Attribs.fold ~init:t ~f:(fun t attrib ->
        let lane_attrib = Attrib.to_lane_attrib attrib in
        merge_potential lane_attrib t
      ) attribs in
      let kernel_attribs_all = KernelAttribs.insert item attribs kernel_attribs_all in
      {t with kernel_attribs_all}
    ) kernel_attribs_all

let union {all=a0; definite=d0; kernel_attribs_all=ka0}
  {all=a1; definite=d1; kernel_attribs_all=ka1} =
  {
    all=Attribs.union a0 a1;
    definite=Attribs.union d0 d1;
    kernel_attribs_all=KernelAttribs.union ka0 ka1
  }

let attribs lr1itemset {kernel_attribs_all; _} =
  KernelAttribs.fold ~init:Attribs.empty
    ~f:(fun attribs (_src_lr1item, src_lr1item_attribs) ->
      Attribs.fold ~init:attribs
        ~f:(fun attribs
          (Attrib.{conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib} as
            attrib) ->
          assert Contrib.(inter conflict contrib = contrib);
          let shift_contrib = Contrib.(inter shift conflict) in
          let shift_attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
              ~isucc_lr1itemset ~contrib:shift_contrib in
          let has_shift = Contrib.is_empty shift_contrib in
          Lr1Itemset.fold ~init:attribs ~f:(fun attribs isucc_lr1item ->
            match Lr1Itemset.get isucc_lr1item lr1itemset with
            | None -> begin
                match has_shift with
                | false -> attribs
                | true ->
                  Attribs.insert shift_attrib attribs
              end
            | Some {follow; _} -> begin
                match Ordset.mem symbol_index follow with
                | false -> begin
                    match has_shift with
                    | false -> attribs
                    | true ->
                      Attribs.insert shift_attrib attribs
                  end
                | true -> begin
                    let attrib' = Attrib.union shift_attrib attrib in
                    Attribs.insert attrib' attribs
                  end
              end
          ) isucc_lr1itemset
        ) src_lr1item_attribs
    ) kernel_attribs_all
