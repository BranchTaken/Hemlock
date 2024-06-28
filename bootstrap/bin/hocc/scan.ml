open Basis
open! Basis.Rudiments

module AbstractToken = struct
  type t =
    | Tok_hocc
    | Tok_token
    | Tok_nonterm
    | Tok_start
    | Tok_epsilon
    | Tok_neutral
    | Tok_left
    | Tok_right
    | Tok_prec

  let pp t formatter =
    formatter |> Fmt.fmt (match t with
      | Tok_hocc -> "Tok_hocc"
      | Tok_token -> "Tok_token"
      | Tok_nonterm -> "Tok_nonterm"
      | Tok_start -> "Tok_start"
      | Tok_epsilon -> "Tok_epsilon"
      | Tok_neutral -> "Tok_neutral"
      | Tok_left -> "Tok_left"
      | Tok_right -> "Tok_right"
      | Tok_prec -> "Tok_prec"
    )

  let malformations = function
    | Tok_hocc | Tok_token | Tok_nonterm | Tok_start | Tok_epsilon
    | Tok_neutral | Tok_left | Tok_right | Tok_prec
      -> []
end

module ConcreteToken = struct
  type t = {
    atok: AbstractToken.t;
    source: Hmc.Source.Slice.t;
  }

  let atok t =
    t.atok

  let source t =
    t.source

  let pp t formatter =
    formatter
    |> Fmt.fmt "{atok=" |> AbstractToken.pp t.atok
    |> Fmt.fmt "; source=" |> Hmc.Source.Slice.pp t.source
    |> Fmt.fmt "}"
end

module Token = struct
  type t =
    | HmcToken of Hmc.Scan.ConcreteToken.t
    | HoccToken of ConcreteToken.t

  let source = function
    | HmcToken ctok -> Hmc.Scan.ConcreteToken.source ctok
    | HoccToken ctok -> ConcreteToken.source ctok

  let pp t formatter =
    match t with
    | HmcToken ctok -> formatter |> Fmt.fmt "HmcToken " |> Hmc.Scan.ConcreteToken.pp ctok
    | HoccToken ctok -> formatter |> Fmt.fmt "HoccToken " |> ConcreteToken.pp ctok

  let malformations = function
    | HmcToken {atok; _} -> Hmc.Scan.AbstractToken.malformations atok
    | HoccToken {atok; _} -> AbstractToken.malformations atok
end

type t = {
  scan: Hmc.Scan.t;
  next: (t * Token.t) Lazy.t;
}

let pp {scan; _} formatter =
  Hmc.Scan.pp scan formatter

let rec susp_next scan = lazy begin
  let scan', ctok = Hmc.Scan.next scan in
  let ctok' = match Hmc.Scan.ConcreteToken.atok ctok with
    | Tok_uident (Constant uident) -> begin
        let open AbstractToken in
        let source = Hmc.Scan.ConcreteToken.source ctok in
        match uident with
        | "hocc" -> Token.HoccToken {atok=Tok_hocc; source}
        | "token" -> Token.HoccToken {atok=Tok_token; source}
        | "nonterm" -> Token.HoccToken {atok=Tok_nonterm; source}
        | "start" -> Token.HoccToken {atok=Tok_start; source}
        | "epsilon" -> Token.HoccToken {atok=Tok_epsilon; source}
        | "neutral" -> Token.HoccToken {atok=Tok_neutral; source}
        | "left" -> Token.HoccToken {atok=Tok_left; source}
        | "right" -> Token.HoccToken {atok=Tok_right; source}
        | "prec" -> Token.HoccToken {atok=Tok_prec; source}
        | _ -> Token.HmcToken ctok
      end
    | _ -> Token.HmcToken ctok
  in
  let t' = {scan=scan'; next=susp_next scan'} in
  t', ctok'
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
