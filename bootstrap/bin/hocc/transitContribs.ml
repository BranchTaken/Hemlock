open Basis
open! Basis.Rudiments

type t = {
  (* Union of conflict contributions in `kernel_contribs`. *)
  all: Attribs.t;

  (* Direct conflict contributions, whether shift (conflict state only) or reduce. *)
  direct: Attribs.t;

  (* Per kernel item reduce conflict contributions. Shift contributions are omitted since it is
   * irrelevant which kernel item makes a shift contribution, whether directly or indirectly. *)
  kernel_contribs: KernelContribs.t;
}

let fmt_hr symbols prods ?(alt=false) ?(width=0L) {direct; all; kernel_contribs} formatter =
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
  |> Fmt.fmt sep |> Fmt.fmt "direct="
  |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) direct
  |> Fmt.fmt sep |> Fmt.fmt "kernel_contribs="
  |> KernelContribs.fmt_hr symbols prods ~alt ~width:(width + 4L) kernel_contribs
  |> Fmt.fmt rsep
  |> Fmt.fmt "}"

let pp {direct; all; kernel_contribs} formatter =
  formatter
  |> Fmt.fmt "{all=" |> Attribs.pp all
  |> Fmt.fmt "; direct=" |> Attribs.pp direct
  |> Fmt.fmt "; kernel_contribs=" |> KernelContribs.pp kernel_contribs
  |> Fmt.fmt "}"

let empty = {
  all=Attribs.empty;
  direct=Attribs.empty;
  kernel_contribs=KernelContribs.empty
}

let reindex index_map {all; direct; kernel_contribs} =
  {
    all=Attribs.reindex index_map all;
    direct=Attribs.reindex index_map direct;
    kernel_contribs=KernelContribs.reindex index_map kernel_contribs
  }

let all {all; _} =
  all

let direct {direct; _} =
  direct

let kernel_contribs {kernel_contribs; _} =
  kernel_contribs

let merge ~conflict_state_index ~symbol_index ~conflict ~contrib ({all; _} as t) =
  let attrib = Attrib.init_anon ~conflict_state_index ~symbol_index ~conflict ~contrib in
  let all = Attribs.insert attrib all in
  {t with all}

let of_anon_attribs anon_attribs =
  Attribs.fold ~init:empty
    ~f:(fun t Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
      merge ~conflict_state_index ~symbol_index ~conflict ~contrib t
    ) anon_attribs

let merge_direct ~conflict_state_index ~symbol_index ~conflict ~contrib ({direct; _} as t) =
  let t = merge ~conflict_state_index ~symbol_index ~conflict ~contrib t in
  let attrib = Attrib.init_anon ~conflict_state_index ~symbol_index ~conflict ~contrib in
  let direct = Attribs.insert attrib direct in
  {t with direct}

let of_anon_attribs_direct anon_attribs_direct =
  Attribs.fold ~init:empty
    ~f:(fun t Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
      merge_direct ~conflict_state_index ~symbol_index ~conflict ~contrib t
    ) anon_attribs_direct

let insert_kernel_contribs kernel_contribs t =
  KernelContribs.fold ~init:t
    ~f:(fun ({kernel_contribs; _} as t) (item, attribs) ->
      let t = Attribs.fold ~init:t
          ~f:(fun t Attrib.{conflict_state_index; symbol_index; conflict; contrib; _} ->
            merge ~conflict_state_index ~symbol_index ~conflict ~contrib t
          ) attribs in
      let kernel_contribs = KernelContribs.insert item attribs kernel_contribs in
      {t with kernel_contribs}
    ) kernel_contribs

let union {all=a0; direct=d0; kernel_contribs=kc0}
  {all=a1; direct=d1; kernel_contribs=kc1} =
  {
    all=Attribs.union a0 a1;
    direct=Attribs.union d0 d1;
    kernel_contribs=KernelContribs.union kc0 kc1
  }

let attribs lr1itemset {kernel_contribs; _} =
  KernelContribs.fold ~init:Attribs.empty
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
    ) kernel_contribs
