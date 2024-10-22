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
