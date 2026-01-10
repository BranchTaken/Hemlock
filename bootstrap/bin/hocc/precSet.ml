open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  index: Index.t;
  names: string array;
  assoc: Assoc.t option;
  doms: (Index.t, Index.cmper_witness) Ordset.t;
  stmt: Parse.nonterm_prec_set;
}

let pp {index; names; assoc; doms; stmt} formatter =
  formatter
  |> Fmt.fmt "{index=" |> Index.pp index
  |> Fmt.fmt "; names=" |> Array.pp String.pp names
  |> Fmt.fmt "; assoc=" |> (Option.pp Assoc.pp) assoc
  |> Fmt.fmt "; doms=" |> Ordset.pp doms
  |> Fmt.fmt "; stmt=" |> Parse.fmt_prec_set stmt
  |> Fmt.fmt "}"

let pp_hr {names; _} formatter =
  formatter
  |> Fmt.fmt "prec "
  |> Fmt.fmt (String.join ~sep:", " (Array.to_list names))

let src_fmt {names; assoc; stmt; _} formatter =
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
  |> Fmt.fmt (String.join ~sep:", " (Array.to_list names))
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

let init ~index ~names ~assoc ~doms ~stmt =
  {index; names; assoc; doms; stmt}
