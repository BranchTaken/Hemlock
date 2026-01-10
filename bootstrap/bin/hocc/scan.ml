open Basis
open! Basis.Rudiments

module Token = struct
  type t =
    | HmcToken of Hmc.Scan.Token.t
    | Tok_hocc of {source: Hmc.Source.Slice.t}
    | Tok_token of {source: Hmc.Source.Slice.t}
    | Tok_nonterm of {source: Hmc.Source.Slice.t}
    | Tok_start of {source: Hmc.Source.Slice.t}
    | Tok_epsilon of {source: Hmc.Source.Slice.t}
    | Tok_neutral of {source: Hmc.Source.Slice.t}
    | Tok_left of {source: Hmc.Source.Slice.t}
    | Tok_right of {source: Hmc.Source.Slice.t}
    | Tok_nonassoc of {source: Hmc.Source.Slice.t}
    | Tok_prec of {source: Hmc.Source.Slice.t}
    | Tok_colon_colon_eq of {source: Hmc.Source.Slice.t}

  let source = function
    | HmcToken tok -> Hmc.Scan.Token.source tok
    | Tok_hocc {source}
    | Tok_token {source}
    | Tok_nonterm {source}
    | Tok_start {source}
    | Tok_epsilon {source}
    | Tok_neutral {source}
    | Tok_left {source}
    | Tok_right {source}
    | Tok_nonassoc {source}
    | Tok_prec {source}
    | Tok_colon_colon_eq {source} -> source

  let pp t formatter =
    match t with
    | HmcToken tok -> formatter |> Fmt.fmt "HmcToken (" |> Hmc.Scan.Token.pp tok |> Fmt.fmt ")"
    | Tok_hocc {source} ->
      formatter |> Fmt.fmt "Tok_hocc {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_token {source} ->
      formatter |> Fmt.fmt "Tok_token {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_nonterm {source} ->
      formatter |> Fmt.fmt "Tok_nonterm {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_start {source} ->
      formatter |> Fmt.fmt "Tok_source {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_epsilon {source} ->
      formatter |> Fmt.fmt "Tok_epsilon {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_neutral {source} ->
      formatter |> Fmt.fmt "Tok_neutral {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_left {source} ->
      formatter |> Fmt.fmt "Tok_left {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_right {source} ->
      formatter |> Fmt.fmt "Tok_right {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_nonassoc {source} ->
      formatter |> Fmt.fmt "Tok_nonassoc {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_prec {source} ->
      formatter |> Fmt.fmt "Tok_prec {source=" |> Hmc.Source.Slice.pp source |> Fmt.fmt "}"
    | Tok_colon_colon_eq {source} ->
      formatter |> Fmt.fmt "Tok_colon_colon_eq {source=" |> Hmc.Source.Slice.pp source
      |> Fmt.fmt "}"

  let malformations = function
    | HmcToken tok -> Hmc.Scan.Token.malformations tok
    | _ -> []
end

type t = {
  scan: Hmc.Scan.t;
  next: (t * Token.t) Lazy.t;
}

let pp {scan; _} formatter =
  Hmc.Scan.pp scan formatter

let rec susp_next scan = lazy begin
  let scan', tok = Hmc.Scan.next scan in
  let tok' = match tok with
    | Tok_uident {uident=Constant uident; _} -> begin
        let source = Hmc.Scan.Token.source tok in
        match uident with
        | "hocc" -> Token.Tok_hocc {source}
        | "token" -> Tok_token {source}
        | "nonterm" -> Tok_nonterm {source}
        | "start" -> Tok_start {source}
        | "epsilon" -> Tok_epsilon {source}
        | "neutral" -> Tok_neutral {source}
        | "left" -> Tok_left {source}
        | "right" -> Tok_right {source}
        | "nonassoc" -> Tok_nonassoc {source}
        | "prec" -> Tok_prec {source}
        | _ -> Token.HmcToken tok
      end
    | Tok_colon_op {colon_op="::="; _} -> begin
        let source = Hmc.Scan.Token.source tok in
        Tok_colon_colon_eq {source}
      end
    | _ -> Token.HmcToken tok
  in
  let t' = {scan=scan'; next=susp_next scan'} in
  t', tok'
end

let init text =
  let scan = Hmc.Scan.init text in
  let next = susp_next scan in
  {scan; next}

let text {scan; _} =
  Hmc.Scan.text scan

let cursor {scan; _} =
  Hmc.Scan.cursor scan

let next {next; _} =
  Lazy.force next
