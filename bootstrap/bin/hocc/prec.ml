open Basis
open! Basis.Rudiments

module Index = Uns
type t = {
  index: Index.t;
  name: string;
  assoc: Assoc.t option;
  doms: (Index.t, Index.cmper_witness) Ordset.t;
  stmt: Parse.nonterm_prec;
}

let pp {index; name; assoc; doms; stmt} formatter =
  formatter
  |> Fmt.fmt "{index=" |> Index.pp index
  |> Fmt.fmt "; name=" |> String.pp name
  |> Fmt.fmt "; assoc=" |> (Option.pp Assoc.pp) assoc
  |> Fmt.fmt "; doms=" |> Ordset.pp doms
  |> Fmt.fmt "; stmt=" |> Parse.fmt_prec stmt
  |> Fmt.fmt "}"

let pp_hr {name; _} formatter =
  formatter
  |> Fmt.fmt "prec "
  |> Fmt.fmt name

let src_fmt {name; assoc; stmt; _} formatter =
  let string_of_token token = begin
    Hmc.Source.Slice.to_string (Scan.Token.source token)
  end in
  formatter
  |> Fmt.fmt (match assoc with
    | None -> "    neutral "
    | Some Left -> "    left "
    | Some Right -> "    right "
  )
  |> Fmt.fmt name
  |> (fun formatter ->
    match stmt with
    | Prec {prec_rels=PrecRelsPrecs {precs=Precs {uident; precs_tl}}; _} -> begin
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
    | Prec {prec_rels=PrecRelsEpsilon; _} -> formatter
  )
  |> Fmt.fmt "\n"

let init ~index ~name ~assoc ~doms ~stmt =
  {index; name; assoc; doms; stmt}
