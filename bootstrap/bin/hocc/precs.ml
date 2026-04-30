open Basis
open! Basis.Rudiments

type t = {
  names: (string, PrecSet.Index.t, String.cmper_witness) Map.t;
  prec_sets: (PrecSet.Index.t, PrecSet.t, PrecSet.Index.cmper_witness) Ordmap.t;
}

let empty = {
  names=Map.empty (module String);
  prec_sets=Ordmap.empty (module PrecSet.Index);
}

let length {prec_sets; _} =
  Ordmap.fold ~init:0L ~f:(fun nprecs (_prec_index, PrecSet.{names; _}) ->
    nprecs + (Array.length names)
  ) prec_sets

let insert ~names ~assoc ~doms ~stmt ({names=names_map; prec_sets} as t) =
  let index = length t in
  let prec_set = PrecSet.init ~index ~names ~assoc ~doms ~stmt in
  let names_map' = Array.fold ~init:names_map ~f:(fun names_map name ->
    Map.insert_hlt ~k:name ~v:index names_map
  ) names in
  let prec_sets' = Ordmap.insert_hlt ~k:index ~v:prec_set prec_sets in
  {names=names_map'; prec_sets=prec_sets'}

let use_assoc Prec.{prec_set_index; _} ({prec_sets; _} as t) =
  let prec_set = Ordmap.get_hlt prec_set_index prec_sets in
  let prec_set' = PrecSet.use_assoc prec_set in
  let prec_sets' = Ordmap.update_hlt ~k:prec_set.index ~v:prec_set' prec_sets in
  {t with prec_sets=prec_sets'}

let prec_index_of_name name {names; _} =
  Map.get name names

let prec_set_of_name name ({prec_sets; _} as t) =
  match prec_index_of_name name t with
  | None -> None
  | Some prec_index -> Ordmap.get prec_index prec_sets

let prec_of_name name t =
  match prec_set_of_name name t with
  | None -> None
  | Some prec_set -> Some (Prec.init ~name ~prec_set)

let prec_set_of_prec_index prec_index {prec_sets; _} =
  Ordmap.get_hlt prec_index prec_sets

let fold_prec_sets ~init ~f {prec_sets; _} =
  Ordmap.fold ~init ~f:(fun accum (_, prec) -> f accum prec) prec_sets

let src_fmt Prec.{name_index; prec_set_index} t formatter =
  let PrecSet.{assoc; stmt; _} as prec_set = prec_set_of_prec_index prec_set_index t in
  let name = PrecSet.name_of_name_index name_index prec_set in
  let string_of_token token = begin
    Hmc.Source.Slice.to_string (Scan.Token.source token)
  end in
  formatter
  |> Fmt.fmt (match assoc with
    | None -> "    neutral "
    | Some Left -> "    left "
    | Some Right -> "    right "
    | Some Nonassoc -> "    nonassoc "
  )
  |> Fmt.fmt name
  |> (fun formatter ->
    match stmt with
    | PrecSet {prec_rels=PrecRelsPrecs {precs=Precs {uident; precs_tl}}; _} -> begin
        let rec fmt_precs_tl precs_tl formatter = begin
          match precs_tl with
          | Parse.PrecsTlUident {uident; precs_tl} -> begin
              formatter
              |> Fmt.fmt ", " |> Fmt.fmt (string_of_token uident)
              |> fmt_precs_tl precs_tl
            end
          | PrecsTlEpsilon -> formatter
        end in
        formatter
        |> Fmt.fmt " < " |> Fmt.fmt (string_of_token uident)
        |> fmt_precs_tl precs_tl
      end
    | PrecSet {prec_rels=PrecRelsEpsilon; _} -> formatter
  )
  |> Fmt.fmt "\n"

let pp_prec_hr Prec.{name_index; prec_set_index} t formatter =
  let prec_set = prec_set_of_prec_index prec_set_index t in
  let name = PrecSet.name_of_name_index name_index prec_set in
  formatter
  |> Fmt.fmt "prec "
  |> Fmt.fmt name
