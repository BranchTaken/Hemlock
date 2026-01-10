open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  name_index: Index.t;
  prec_set: PrecSet.t;
}

let name {name_index; prec_set={names; _}} =
  Array.get name_index names

let pp {name_index; prec_set} formatter =
  formatter
  |> Fmt.fmt "{name_index=" |> Index.pp name_index
  |> Fmt.fmt "; prec_set=" |> PrecSet.pp prec_set

let pp_hr t formatter =
  formatter
  |> Fmt.fmt "prec "
  |> Fmt.fmt (name t)

let src_fmt ({prec_set={assoc; stmt; _}; _} as t) formatter =
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
  |> Fmt.fmt (name t)
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
  {name_index; prec_set}
