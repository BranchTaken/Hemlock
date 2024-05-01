open Basis
open! Basis.Rudiments

type t = {
  (* Union of conflict contributions in `kernel_contribs`. *)
  all: AnonContribs.t;

  (* Direct conflict contributions, whether shift (conflict state only) or reduce. *)
  direct: AnonContribs.t;

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
  |> Fmt.fmt sep |> Fmt.fmt "all=" |> AnonContribs.fmt_hr symbols prods ~alt ~width:(width + 4L) all
  |> Fmt.fmt sep |> Fmt.fmt "direct="
  |> AnonContribs.fmt_hr symbols prods ~alt ~width:(width + 4L) direct
  |> Fmt.fmt sep |> Fmt.fmt "kernel_contribs="
  |> KernelContribs.fmt_hr symbols prods ~alt ~width:(width + 4L) kernel_contribs
  |> Fmt.fmt rsep
  |> Fmt.fmt "}"

let pp {direct; all; kernel_contribs} formatter =
  formatter
  |> Fmt.fmt "{all=" |> AnonContribs.pp all
  |> Fmt.fmt "; direct=" |> AnonContribs.pp direct
  |> Fmt.fmt "; kernel_contribs=" |> KernelContribs.pp kernel_contribs
  |> Fmt.fmt "}"

let empty = {
  all=AnonContribs.empty;
  direct=AnonContribs.empty;
  kernel_contribs=KernelContribs.empty
}

let reindex index_map {all; direct; kernel_contribs} =
  {
    all=AnonContribs.reindex index_map all;
    direct=AnonContribs.reindex index_map direct;
    kernel_contribs=KernelContribs.reindex index_map kernel_contribs
  }

let all {all; _} =
  all

let direct {direct; _} =
  direct

let kernel_contribs {kernel_contribs; _} =
  kernel_contribs

let merge ~conflict_state_index symbol_index contrib ({all; _} as t) =
  let all = AnonContribs.insert ~conflict_state_index symbol_index contrib all in
  {t with all}

let of_anon_contribs anon_contribs =
  AnonContribs.fold ~init:empty ~f:(fun t conflict_state_index symbol_index contrib ->
    merge ~conflict_state_index symbol_index contrib t
  ) anon_contribs

let merge_direct ~conflict_state_index symbol_index contrib ({direct; _} as t) =
  let t = merge ~conflict_state_index symbol_index contrib t in
  let direct = AnonContribs.insert ~conflict_state_index symbol_index contrib direct in
  {t with direct}

let of_anon_contribs_direct anon_contribs_direct =
  AnonContribs.fold ~init:empty ~f:(fun t conflict_state_index symbol_index v ->
    merge_direct ~conflict_state_index symbol_index v t
  ) anon_contribs_direct

let insert_kernel_contribs kernel_contribs t =
  KernelContribs.fold ~init:t
    ~f:(fun ({kernel_contribs; _} as t) (item, contribs) ->
      let t = Contribs.fold ~init:t
          ~f:(fun t conflict_state_index Attrib.{symbol_index; contrib; _} ->
            merge ~conflict_state_index symbol_index contrib t
          ) contribs in
      let kernel_contribs = KernelContribs.insert item contribs kernel_contribs in
      {t with kernel_contribs}
    ) kernel_contribs

let union {all=a0; direct=d0; kernel_contribs=kc0}
  {all=a1; direct=d1; kernel_contribs=kc1} =
  {
    all=AnonContribs.union a0 a1;
    direct=AnonContribs.union d0 d1;
    kernel_contribs=KernelContribs.union kc0 kc1
  }

let contribs lr1itemset {kernel_contribs; _} =
  KernelContribs.fold ~init:Contribs.empty
    ~f:(fun contribs (_src_lr1item, src_lr1item_contribs) ->
      Contribs.fold ~init:contribs
        ~f:(fun contribs conflict_state_index
          (Attrib.{symbol_index; conflict; isucc_lr1itemset; contrib} as attrib) ->
          assert Contrib.(inter conflict contrib = contrib);
          let shift_contrib = Contrib.(inter shift conflict) in
          let shift_attrib =
            Attrib.init ~symbol_index ~conflict ~isucc_lr1itemset ~contrib:shift_contrib in
          let has_shift = Contrib.is_empty shift_contrib in
          Lr1Itemset.fold ~init:contribs ~f:(fun contribs isucc_lr1item ->
            match Lr1Itemset.get isucc_lr1item lr1itemset with
            | None -> begin
                match has_shift with
                | false -> contribs
                | true ->
                  Contribs.insert ~conflict_state_index shift_attrib contribs
              end
            | Some {follow; _} -> begin
                match Ordset.mem symbol_index follow with
                | false -> begin
                    match has_shift with
                    | false -> contribs
                    | true ->
                      Contribs.insert ~conflict_state_index shift_attrib contribs
                  end
                | true -> begin
                    let attrib' = Attrib.union shift_attrib attrib in
                    Contribs.insert ~conflict_state_index attrib' contribs
                  end
              end
          ) isucc_lr1itemset
        ) src_lr1item_contribs
    ) kernel_contribs
  |> Contribs.merged_of_t
