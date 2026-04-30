open Basis
open! Basis.Rudiments

module Index = PrecIndex
type t = {
  name_index: Index.t;
  prec_set_index: PrecSet.Index.t;
}

let pp {name_index; prec_set_index} formatter =
  formatter
  |> Fmt.fmt "{name_index=" |> Index.pp name_index
  |> Fmt.fmt "; prec_set_index=" |> PrecSet.Index.pp prec_set_index

let init ~name ~prec_set =
  let PrecSet.{names; _} = prec_set in
  let name_index =
    Array.findi_map ~f:(fun i s ->
      match String.(name = s) with
      | true -> Some i
      | false -> None
    ) names
    |> Option.value_hlt
  in
  let prec_set_index = prec_set.index in
  {name_index; prec_set_index}
