(** Recursive descent parser for the hocc grammar that is documented (using hocc syntax) in the
    manual. This could in principle be bootstrapped to be hocc-generated, but doing so would
    complicate maintenance more than it's probably worth. However, it is useful to keep the
    `Hocc.hmh` test grammar in sync in order to avoid introducing grammar ambiguities.

    Note that `trace` can be set to `true` in order to enable extremely verbose output, should the
    need to diagnose a parser flaw arise. *)

open Basis
open! Basis.Rudiments

module Error = struct
  module T = struct
    type t = {
      source: Hmc.Source.Slice.t;
      msg: string;
    }

    let cmp t0 t1 =
      Hmc.Source.Slice.cmp t0.source t1.source

    let pp {source; msg} formatter =
      formatter
      |> Fmt.fmt "{source=" |> Hmc.Source.Slice.pp source
      |> Fmt.fmt "; msg=" |> String.pp msg
      |> Fmt.fmt "}"

    let fmt ?(alt=false) ({source; msg} as t) formatter =
      match alt with
      | false -> pp t formatter
      | true -> begin
          formatter
          |> Fmt.fmt "hocc: At "
          |> Hmc.Source.Slice.pp source
          |> Fmt.fmt ": "
          |> Fmt.fmt msg
          |> Fmt.fmt "\n"
        end
  end
  include T
  include Cmpable.Make(T)

  let init_token token msg =
    {source=Scan.Token.source token; msg}

  let init_mal mal =
    let open Hmc.Scan.AbstractToken.Rendition.Malformation in
    {source=source mal; msg=description mal}

  let init_scanner scanner msg =
    let cursor = Scan.cursor scanner in
    let source = Hmc.Source.Slice.of_cursors ~base:cursor ~past:cursor in
    {source; msg}
end

type uident =
  | Uident of {uident: Scan.Token.t}
and cident =
  | Cident of {cident: Scan.Token.t}
and ident =
  | IdentUident of {uident: uident}
  | IdentCident of {cident: cident}
  | IdentUscore of {uscore: Scan.Token.t}
and precs_tl =
  | PrecsTlCommaUident of {comma: Scan.Token.t; uident: uident; precs_tl: precs_tl}
  | PrecsTlEpsilon
and precs =
  | Precs of {uident: uident; precs_tl: precs_tl}
and prec_rels =
  | PrecRelsLtPrecs of {lt: Scan.Token.t; precs: precs}
  | PrecRelsEpsilon
and prec_type =
  | PrecTypeNeutral of {neutral: Scan.Token.t}
  | PrecTypeLeft of {left: Scan.Token.t}
  | PrecTypeRight of {right: Scan.Token.t}
and prec =
  | Prec of {prec_type: prec_type; uident: uident; prec_rels: prec_rels}
and of_type =
  | OfType of {of_: Scan.Token.t; type_module: cident; dot: Scan.Token.t; type_type: uident}
and of_type0 =
  | OfType0OfType of {of_type: of_type}
  | OfType0Epsilon
and prec_ref =
  | PrecRefPrecUident of {prec: Scan.Token.t; uident: uident}
  | PrecRefEpsilon
and token_alias =
  | TokenAlias of {alias: Scan.Token.t}
  | TokenAliasEpsilon
and token =
  | Token of {token: Scan.Token.t; cident: cident; token_alias: token_alias; of_type0: of_type0;
    prec_ref: prec_ref}
and sep =
  | SepLineDelim of {line_delim: Scan.Token.t}
  | SepSemi of {semi: Scan.Token.t}
  | SepBar of {bar: Scan.Token.t}
and codes_tl =
  | CodesTlSepCode of {sep: sep; code: code; codes_tl: codes_tl}
  | CodesTlEpsilon
and codes =
  | Codes of {code: code; codes_tl: codes_tl}
and codes0 =
  | Codes0Codes of {codes: codes}
  | Codes0Epsilon
and delimited =
  | DelimitedBlock of {indent: Scan.Token.t; codes: codes; dedent: Scan.Token.t}
  | DelimitedParen of {lparen: Scan.Token.t; codes0: codes0; rparen: Scan.Token.t}
  | DelimitedCapture of {lcapture: Scan.Token.t; codes0: codes0; rcapture: Scan.Token.t}
  | DelimitedList of {lbrack: Scan.Token.t; codes0: codes0; rbrack: Scan.Token.t}
  | DelimitedArray of {larray: Scan.Token.t; codes0: codes0; rarray: Scan.Token.t}
  | DelimitedModule of {lcurly: Scan.Token.t; codes0: codes0; rcurly: Scan.Token.t}
and code_tl =
  | CodeTlDelimited of {delimited: delimited; code_tl: code_tl}
  | CodeTlToken of {token: Scan.Token.t; code_tl: code_tl}
  | CodeTlEpsilon
and code =
  | CodeDelimited of {delimited: delimited; code_tl: code_tl}
  | CodeToken of {token: Scan.Token.t; code_tl: code_tl}
and prod_param_symbol =
  | ProdParamSymbolCident of {cident: cident}
  | ProdParamSymbolAlias of {alias: Scan.Token.t}
and prod_param =
  | ProdParamBinding of {ident: ident; colon: Scan.Token.t; prod_param_symbol: prod_param_symbol}
  | ProdParam of {prod_param_symbol: prod_param_symbol}
and prod_params_tl =
  | ProdParamsTlProdParam of {prod_param: prod_param; prod_params_tl: prod_params_tl}
  | ProdParamsTlEpsilon
and prod_params =
  | ProdParamsProdParam of {prod_param: prod_param; prod_params_tl: prod_params_tl}
and prod_pattern =
  | ProdPatternParams of {prod_params: prod_params}
  | ProdPatternEpsilon of {epsilon: Scan.Token.t}
and prod =
  | Prod of {prod_pattern: prod_pattern; prec_ref: prec_ref}
and prods_tl =
  | ProdsTlBarProd of {bar: Scan.Token.t; prod: prod; prods_tl: prods_tl}
  | ProdsTlEpsilon
and prods =
  | ProdsBarProd of {bar: Scan.Token.t; prod: prod; prods_tl: prods_tl}
  | ProdsProd of {prod: prod; prods_tl: prods_tl}
and reduction =
  | Reduction of {prods: prods; arrow: Scan.Token.t; code: code}
and reductions_tl =
  | ReductionsTlBarReduction of {bar: Scan.Token.t; reduction: reduction;
    reductions_tl: reductions_tl}
  | ReductionsTlEpsilon
and reductions =
  | ReductionsReduction of {reduction: reduction; reductions_tl: reductions_tl}
and nonterm_type =
  | NontermTypeNonterm of {nonterm: Scan.Token.t}
  | NontermTypeStart of {start: Scan.Token.t}
and nonterm =
  | NontermProds of {nonterm_type: nonterm_type; cident: cident; prec_ref: prec_ref;
    cce: Scan.Token.t; prods: prods}
  | NontermReductions of {nonterm_type: nonterm_type; cident: cident; of_type: of_type;
      prec_ref: prec_ref; cce: Scan.Token.t; reductions: reductions}
and stmt =
  | StmtPrec of {prec: prec}
  | StmtToken of {token: token}
  | StmtNonterm of {nonterm: nonterm}
  | StmtCode of {code: code}
and stmts_tl =
  | StmtsTl of {line_delim: Scan.Token.t; stmt: stmt; stmts_tl: stmts_tl}
  | StmtsTlEpsilon
and stmts =
  | Stmts of {stmt: stmt; stmts_tl: stmts_tl}
and hocc =
  | Hocc of {hocc: Scan.Token.t; indent: Scan.Token.t; stmts: stmts; dedent: Scan.Token.t}
and eoi =
  | Eoi of {eoi: Scan.Token.t}
and matter =
  | Matter of {token: Scan.Token.t; matter: matter}
  | MatterEpsilon
and hmh =
  | Hmh of {prelude: matter; hocc: hocc; postlude: matter; eoi: eoi}
and hmhi =
  | Hmhi of {prelude: matter; hocc: Scan.Token.t; postlude: matter; eoi: eoi}

(**************************************************************************************************)
(* source_of_* functions. *)

(* Not to be confused with joining forces. *)
let join_sources source0_opt source1_opt =
  match source0_opt, source1_opt with
  | None, None -> None
  | Some _, None -> source0_opt
  | None, Some _ -> source1_opt
  | Some source0, Some source1 -> begin
      let open Hmc.Source in
      let base0, past0 = Slice.cursors source0 in
      let base1, past1 = Slice.cursors source1 in
      let open Cmp in
      let base = match Cursor.cmp base0 base1 with
        | Lt
        | Eq -> base0
        | Gt -> base1
      in
      let past = match Cursor.cmp past0 past1 with
        | Lt
        | Eq -> past1
        | Gt -> past0
      in
      Some (Slice.of_cursors ~base ~past)
    end

(* Not to be confused with a token force. *)
let token_source token =
  Some (Scan.Token.source token)

let rec source_of_uident = function
  | Uident {uident} -> token_source uident

and source_of_cident = function
  | Cident {cident} -> token_source cident

and source_of_ident = function
  | IdentUident {uident} -> source_of_uident uident
  | IdentCident {cident} -> source_of_cident cident
  | IdentUscore {uscore} -> token_source uscore

and source_of_precs_tl = function
  | PrecsTlCommaUident {comma; uident; precs_tl} ->
    token_source comma
    |> join_sources (source_of_uident uident)
    |> join_sources (source_of_precs_tl precs_tl)
  | PrecsTlEpsilon -> None

and source_of_precs = function
  | Precs {uident; precs_tl} ->
    source_of_uident uident
    |> join_sources (source_of_precs_tl precs_tl)

and source_of_prec_rels = function
  | PrecRelsLtPrecs {lt; precs} ->
    token_source lt
    |> join_sources (source_of_precs precs)
  | PrecRelsEpsilon -> None

and source_of_prec_type = function
  | PrecTypeNeutral {neutral} -> token_source neutral
  | PrecTypeLeft {left} -> token_source left
  | PrecTypeRight {right} -> token_source right

and source_of_prec = function
  | Prec {prec_type; uident; prec_rels} ->
    source_of_prec_type prec_type
    |> join_sources (source_of_uident uident)
    |> join_sources (source_of_prec_rels prec_rels)

and source_of_of_type = function
  | OfType {of_; type_module=_; dot; type_type} ->
    token_source of_
    |> join_sources (token_source dot)
    |> join_sources (source_of_uident type_type)

and source_of_of_type0 = function
  | OfType0OfType {of_type} -> source_of_of_type of_type
  | OfType0Epsilon -> None

and source_of_prec_ref = function
  | PrecRefPrecUident {prec; uident} ->
    token_source prec
    |> join_sources (source_of_uident uident)
  | PrecRefEpsilon -> None

and source_of_token_alias = function
  | TokenAlias {alias} -> token_source alias
  | TokenAliasEpsilon -> None

and source_of_token = function
  | Token {token; cident; token_alias; of_type0; prec_ref} ->
    token_source token
    |> join_sources (source_of_cident cident)
    |> join_sources (source_of_token_alias token_alias)
    |> join_sources (source_of_of_type0 of_type0)
    |> join_sources (source_of_prec_ref prec_ref)

and source_of_sep = function
  | SepLineDelim {line_delim} -> token_source line_delim
  | SepSemi {semi} -> token_source semi
  | SepBar {bar} -> token_source bar

and source_of_codes_tl = function
  | CodesTlSepCode {sep; code; codes_tl} ->
    source_of_sep sep
    |> join_sources (source_of_code code)
    |> join_sources (source_of_codes_tl codes_tl)
  | CodesTlEpsilon -> None

and source_of_codes = function
  | Codes {code; codes_tl} ->
    source_of_code code
    |> join_sources (source_of_codes_tl codes_tl)

and source_of_codes0 = function
  | Codes0Codes {codes} -> source_of_codes codes
  | Codes0Epsilon -> None

and source_of_delimited = function
  | DelimitedBlock {indent=ldelim; codes=_; dedent=rdelim}
  | DelimitedParen {lparen=ldelim; codes0=_; rparen=rdelim}
  | DelimitedCapture {lcapture=ldelim; codes0=_; rcapture=rdelim}
  | DelimitedList {lbrack=ldelim; codes0=_; rbrack=rdelim}
  | DelimitedArray {larray=ldelim; codes0=_; rarray=rdelim}
  | DelimitedModule {lcurly=ldelim; codes0=_; rcurly=rdelim} ->
    token_source ldelim
    |> join_sources (token_source rdelim)

and source_of_code_tl = function
  | CodeTlDelimited {delimited; code_tl} ->
    source_of_delimited delimited
    |> join_sources (source_of_code_tl code_tl)
  | CodeTlToken {token; code_tl} ->
    token_source token
    |> join_sources (source_of_code_tl code_tl)
  | CodeTlEpsilon -> None

and source_of_code = function
  | CodeDelimited {delimited; code_tl} ->
    source_of_delimited delimited
    |> join_sources (source_of_code_tl code_tl)
  | CodeToken {token; code_tl} ->
    token_source token
    |> join_sources (source_of_code_tl code_tl)

and source_of_prod_param_symbol = function
  | ProdParamSymbolCident {cident} -> source_of_cident cident
  | ProdParamSymbolAlias {alias} -> token_source alias

and source_of_prod_param = function
  | ProdParamBinding {ident; colon=_; prod_param_symbol} ->
    source_of_ident ident
    |> join_sources (source_of_prod_param_symbol prod_param_symbol)
  | ProdParam {prod_param_symbol} ->
    source_of_prod_param_symbol prod_param_symbol

and source_of_prod_params_tl = function
  | ProdParamsTlProdParam {prod_param; prod_params_tl} ->
    source_of_prod_param prod_param
    |> join_sources (source_of_prod_params_tl prod_params_tl)
  | ProdParamsTlEpsilon -> None

and source_of_prod_params = function
  | ProdParamsProdParam {prod_param; prod_params_tl} ->
    source_of_prod_param prod_param
    |> join_sources (source_of_prod_params_tl prod_params_tl)

and source_of_prod_pattern = function
  | ProdPatternParams {prod_params} -> source_of_prod_params prod_params
  | ProdPatternEpsilon {epsilon} -> token_source epsilon

and source_of_prod = function
  | Prod {prod_pattern; prec_ref} ->
    source_of_prod_pattern prod_pattern
    |> join_sources (source_of_prec_ref prec_ref)

and source_of_prods_tl = function
  | ProdsTlBarProd {bar; prod; prods_tl} ->
    token_source bar
    |> join_sources (source_of_prod prod)
    |> join_sources (source_of_prods_tl prods_tl)
  | ProdsTlEpsilon -> None

and source_of_prods = function
  | ProdsBarProd {bar; prod; prods_tl} ->
    token_source bar
    |> join_sources (source_of_prod prod)
    |> join_sources (source_of_prods_tl prods_tl)
  | ProdsProd {prod; prods_tl} ->
    source_of_prod prod
    |> join_sources (source_of_prods_tl prods_tl)

and source_of_reduction = function
  | Reduction {prods; arrow=_; code} ->
    source_of_prods prods
    |> join_sources (source_of_code code)

and source_of_reductions_tl = function
  | ReductionsTlBarReduction {bar; reduction; reductions_tl} ->
    token_source bar
    |> join_sources (source_of_reduction reduction)
    |> join_sources (source_of_reductions_tl reductions_tl)
  | ReductionsTlEpsilon -> None

and source_of_reductions = function
  | ReductionsReduction {reduction; reductions_tl} ->
    source_of_reduction reduction
    |> join_sources (source_of_reductions_tl reductions_tl)

and source_of_nonterm_type = function
  | NontermTypeNonterm {nonterm} -> token_source nonterm
  | NontermTypeStart {start} -> token_source start

and source_of_nonterm = function
  | NontermProds {nonterm_type; cident=_; prec_ref=_; cce=_; prods} ->
    source_of_nonterm_type nonterm_type
    |> join_sources (source_of_prods prods)
  | NontermReductions {nonterm_type; cident=_; of_type=_; prec_ref=_; cce=_; reductions} ->
    source_of_nonterm_type nonterm_type
    |> join_sources (source_of_reductions reductions)

and source_of_stmt = function
  | StmtPrec {prec} -> source_of_prec prec
  | StmtToken {token} -> source_of_token token
  | StmtNonterm {nonterm} -> source_of_nonterm nonterm
  | StmtCode {code} -> source_of_code code

and source_of_stmts_tl = function
  | StmtsTl {line_delim; stmt; stmts_tl} ->
    token_source line_delim
    |> join_sources (source_of_stmt stmt)
    |> join_sources (source_of_stmts_tl stmts_tl)
  | StmtsTlEpsilon -> None

and source_of_stmts = function
  | Stmts {stmt; stmts_tl} ->
    source_of_stmt stmt
    |> join_sources (source_of_stmts_tl stmts_tl)

and source_of_hocc = function
  | Hocc {hocc; indent=_; stmts=_; dedent} ->
    token_source hocc
    |> join_sources (token_source dedent)

and source_of_eoi = function
  | Eoi {eoi} -> token_source eoi

and source_of_matter = function
  | Matter {token; matter} ->
    token_source token
    |> join_sources (source_of_matter matter)
  | MatterEpsilon -> None

and source_of_hmh = function
  | Hmh {prelude; hocc; postlude=_; eoi} ->
    source_of_matter prelude
    |> join_sources (source_of_hocc hocc)
    |> join_sources (source_of_eoi eoi)

and source_of_hmhi = function
  | Hmhi {prelude; hocc; postlude=_; eoi} ->
    source_of_matter prelude
    |> join_sources (token_source hocc)
    |> join_sources (source_of_eoi eoi)

(**************************************************************************************************)
(* fmt_* functions. *)

let fmt_lcurly ~alt ~width formatter =
  match alt with
  | false -> formatter |> Fmt.fmt "{"
  | true ->
    formatter
    |> Fmt.fmt "{\n"
    |> Fmt.fmt ~pad:" " ~just:Fmt.Left ~width:(width + 4L) ""

let fmt_semi ~alt ~width formatter =
  match alt with
  | false -> formatter |> Fmt.fmt "; "
  | true ->
    formatter
    |> Fmt.fmt "\n"
    |> Fmt.fmt ~pad:" " ~just:Fmt.Left ~width:(width + 4L) ""

let fmt_rcurly ~alt ~width formatter =
  match alt with
  | false -> formatter |> Fmt.fmt "}"
  | true ->
    formatter
    |> Fmt.fmt "\n"
    |> Fmt.fmt ~pad:" " ~just:Fmt.Left ~width:(width + 2L) ""
    |> Fmt.fmt "}"

let rec fmt_uident ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) uident formatter =
  match uident with
  | Uident {uident} ->
    formatter
    |> Fmt.fmt "Uident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "uident=" |> Scan.Token.pp uident
    |> fmt_rcurly ~alt ~width
and pp_uident uident formatter =
  fmt_uident uident formatter

let rec fmt_cident ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) cident formatter =
  match cident with
  | Cident {cident} ->
    formatter
    |> Fmt.fmt "Cident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "cident=" |> Scan.Token.pp cident
    |> fmt_rcurly ~alt ~width
and pp_cident cident formatter =
  fmt_cident cident formatter

and fmt_ident ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) ident formatter =
  match ident with
  | IdentUident {uident} ->
    formatter |> Fmt.fmt "IdentUident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "uident=" |> pp_uident uident
    |> fmt_rcurly ~alt ~width
  | IdentCident {cident} ->
    formatter |> Fmt.fmt "IdentCident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "cident=" |> pp_cident cident
    |> fmt_rcurly ~alt ~width
  | IdentUscore {uscore} ->
    formatter |> Fmt.fmt "IdentUscore "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "uscore=" |> Scan.Token.pp uscore
    |> fmt_rcurly ~alt ~width
and pp_ident ident formatter =
  fmt_ident ident formatter

and fmt_precs_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) precs_tl
  formatter =
  let width' = width + 4L in
  match precs_tl with
  | PrecsTlCommaUident {comma; uident; precs_tl} ->
    formatter |> Fmt.fmt "PrecsTlCommaUident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "comma=" |> Scan.Token.pp comma
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "uident=" |> fmt_uident ~alt ~width:width' uident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "precs_tl=" |> fmt_precs_tl ~alt ~width:width' precs_tl
    |> fmt_rcurly ~alt ~width
  | PrecsTlEpsilon ->
    formatter |> Fmt.fmt "PrecsTlEpsilon"
and pp_precs_tl precs_tl formatter =
  fmt_precs_tl precs_tl formatter

and fmt_precs ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) precs formatter =
  let width' = width + 4L in
  match precs with
  | Precs {uident; precs_tl} ->
    formatter |> Fmt.fmt "Precs "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "uident=" |> fmt_uident ~alt ~width:width' uident
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "precs_tl=" |> fmt_precs_tl ~alt ~width:width' precs_tl
    |> fmt_rcurly ~alt ~width
and pp_precs precs formatter =
  fmt_precs precs formatter

and fmt_prec_rels ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prec_rels formatter =
  let width' = width + 4L in
  match prec_rels with
  | PrecRelsLtPrecs {lt; precs} ->
    formatter |> Fmt.fmt "PrecRelsLtPrecs "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "lt=" |> Scan.Token.pp lt
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "precs=" |> fmt_precs ~alt ~width:width' precs
    |> fmt_rcurly ~alt ~width
  | PrecRelsEpsilon ->
    formatter |> Fmt.fmt "PrecRelsEpsilon"
and pp_prec_rels prec_rels formatter =
  fmt_prec_rels prec_rels formatter

and fmt_prec_type ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prec_type formatter =
  match prec_type with
  | PrecTypeNeutral {neutral} ->
    formatter |> Fmt.fmt "PrecTypeNeutral "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "neutral=" |> Scan.Token.pp neutral
    |> fmt_rcurly ~alt ~width
  | PrecTypeLeft {left} ->
    formatter |> Fmt.fmt "PrecTypeLeft "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "left=" |> Scan.Token.pp left
    |> fmt_rcurly ~alt ~width
  | PrecTypeRight {right} ->
    formatter |> Fmt.fmt "PrecTypeRight "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "right=" |> Scan.Token.pp right
    |> fmt_rcurly ~alt ~width
and pp_prec_type prec_type formatter =
  fmt_prec_type prec_type formatter

and fmt_prec ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prec formatter =
  let width' = width + 4L in
  match prec with
  | Prec {prec_type; uident; prec_rels} ->
    formatter |> Fmt.fmt "Prec "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prec_type=" |> fmt_prec_type ~alt ~width:width' prec_type
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "uident=" |> fmt_uident ~alt ~width:width' uident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prec_rels=" |> fmt_prec_rels ~alt ~width:width' prec_rels
    |> fmt_rcurly ~alt ~width
and pp_prec prec formatter =
  fmt_prec prec formatter

and fmt_of_type ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) of_type formatter =
  let width' = width + 4L in
  match of_type with
  | OfType {of_; type_module; dot; type_type} ->
    formatter |> Fmt.fmt "OfType "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "of_=" |> Scan.Token.pp of_
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "type_module=" |> fmt_cident ~alt ~width:width' type_module
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "dot=" |> Scan.Token.pp dot
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "type_type=" |> fmt_uident ~alt ~width:width' type_type
    |> fmt_rcurly ~alt ~width
and pp_of_type of_type formatter =
  fmt_of_type of_type formatter

and fmt_of_type0 ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) of_type0 formatter =
  let width' = width + 4L in
  match of_type0 with
  | OfType0OfType {of_type} ->
    formatter |> Fmt.fmt "OfType0OfType "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "of_type=" |> fmt_of_type ~alt ~width:width' of_type
    |> fmt_rcurly ~alt ~width
  | OfType0Epsilon ->
    formatter |> Fmt.fmt "OfType0Epsilon"
and pp_of_type0 of_type0 formatter =
  fmt_of_type0 of_type0 formatter

and fmt_prec_ref ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prec_ref formatter =
  let width' = width + 4L in
  match prec_ref with
  | PrecRefPrecUident {prec; uident} ->
    formatter |> Fmt.fmt "PrecRefPrecUident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prec=" |> Scan.Token.pp prec
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "uident=" |> fmt_uident ~alt ~width:width' uident
    |> fmt_rcurly ~alt ~width
  | PrecRefEpsilon ->
    formatter |> Fmt.fmt "PrecRefEpsilon"
and pp_prec_ref prec_ref formatter =
  fmt_prec_ref prec_ref formatter

and fmt_token_alias ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) token_alias formatter =
  match token_alias with
  | TokenAlias {alias} ->
    formatter |> Fmt.fmt "Token "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "alias=" |> Scan.Token.pp alias
    |> fmt_rcurly ~alt ~width
  | TokenAliasEpsilon ->
    formatter |> Fmt.fmt "TokenAliasEpsilon"
and pp_token_alias token_alias formatter =
  fmt_token_alias token_alias formatter

and fmt_token ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) token formatter =
  let width' = width + 4L in
  match token with
  | Token {token; cident; token_alias; of_type0; prec_ref} ->
    formatter |> Fmt.fmt "Token "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "token=" |> Scan.Token.pp token
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "cident=" |> fmt_cident ~alt ~width:width' cident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "token_alias=" |> fmt_token_alias ~alt ~width:width' token_alias
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "of_type0=" |> fmt_of_type0 ~alt ~width:width' of_type0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prec_ref=" |> fmt_prec_ref ~alt ~width:width' prec_ref
    |> fmt_rcurly ~alt ~width
and pp_token token formatter =
  fmt_token token formatter

and fmt_sep ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) sep formatter =
  match sep with
  | SepLineDelim {line_delim} ->
    formatter |> Fmt.fmt "SepLineDelim "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "line_delim=" |> Scan.Token.pp line_delim
    |> fmt_rcurly ~alt ~width
  | SepSemi {semi} ->
    formatter |> Fmt.fmt "SepSemi "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "semi=" |> Scan.Token.pp semi
    |> fmt_rcurly ~alt ~width
  | SepBar {bar} ->
    formatter |> Fmt.fmt "SepBar "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "bar=" |> Scan.Token.pp bar
    |> fmt_rcurly ~alt ~width
and pp_sep sep formatter =
  fmt_sep sep formatter

and fmt_codes_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) codes_tl formatter =
  let width' = width + 4L in
  match codes_tl with
  | CodesTlSepCode {sep; code; codes_tl} ->
    formatter |> Fmt.fmt "CodesTlSepCode "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "sep=" |> fmt_sep ~alt ~width:width' sep
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code=" |> fmt_code ~alt ~width:width' code
    |> fmt_rcurly ~alt ~width
    |> Fmt.fmt "codes_tl=" |> fmt_codes_tl ~alt ~width:width' codes_tl
    |> fmt_rcurly ~alt ~width
  | CodesTlEpsilon -> formatter |> Fmt.fmt "CodesTlEpsilon"
and pp_codes_tl codes_tl formatter =
  fmt_codes codes_tl formatter

and fmt_codes ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) codes formatter =
  let width' = width + 4L in
  match codes with
  | Codes {code; codes_tl} ->
    formatter |> Fmt.fmt "Codes "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "code=" |> fmt_code ~alt ~width:width' code
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes_tl=" |> fmt_codes_tl ~alt ~width:width' codes_tl
    |> fmt_rcurly ~alt ~width
and pp_codes codes formatter =
  fmt_codes codes formatter

and fmt_codes0 ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) codes0 formatter =
  let width' = width + 4L in
  match codes0 with
  | Codes0Codes {codes} ->
    formatter |> Fmt.fmt "Codes0Codes "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "codes=" |> fmt_codes ~alt ~width:width' codes
    |> fmt_rcurly ~alt ~width
  | Codes0Epsilon ->
    formatter |> Fmt.fmt "Codes0Epsilon"
and pp_codes0 codes formatter =
  fmt_codes codes formatter

and fmt_delimited ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) delimited formatter =
  let width' = width + 4L in
  match delimited with
  | DelimitedBlock {indent; codes; dedent} ->
    formatter |> Fmt.fmt "DelimitedBlock "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "indent=" |> Scan.Token.pp indent
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes=" |> fmt_codes ~alt ~width:width' codes
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "dedent=" |> Scan.Token.pp dedent
    |> fmt_rcurly ~alt ~width
  | DelimitedParen {lparen; codes0; rparen} ->
    formatter |> Fmt.fmt "DelimitedParen "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "lparen=" |> Scan.Token.pp lparen
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes0=" |> fmt_codes0 ~alt ~width:width' codes0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "rparen=" |> Scan.Token.pp rparen
    |> fmt_rcurly ~alt ~width
  | DelimitedCapture {lcapture; codes0; rcapture} ->
    formatter |> Fmt.fmt "DelimitedCapture "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "lcapture=" |> Scan.Token.pp lcapture
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes0=" |> fmt_codes0 ~alt ~width:width' codes0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "rcapture=" |> Scan.Token.pp rcapture
    |> fmt_rcurly ~alt ~width
  | DelimitedList {lbrack; codes0; rbrack} ->
    formatter |> Fmt.fmt "DelimitedList "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "lbrack=" |> Scan.Token.pp lbrack
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes0=" |> fmt_codes0 ~alt ~width:width' codes0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "rbrack=" |> Scan.Token.pp rbrack
    |> fmt_rcurly ~alt ~width
  | DelimitedArray {larray; codes0; rarray} ->
    formatter |> Fmt.fmt "DelimitedArray "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "larray=" |> Scan.Token.pp larray
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes0=" |> fmt_codes0 ~alt ~width:width' codes0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "rarray=" |> Scan.Token.pp rarray
    |> fmt_rcurly ~alt ~width
  | DelimitedModule {lcurly; codes0; rcurly} ->
    formatter |> Fmt.fmt "DelimitedModule "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "lcurly=" |> Scan.Token.pp lcurly
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "codes0=" |> fmt_codes0 ~alt ~width:width' codes0
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "rcurly=" |> Scan.Token.pp rcurly
    |> fmt_rcurly ~alt ~width
and pp_delimited delimited formatter =
  fmt_delimited delimited formatter

and fmt_code_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) code_tl formatter =
  let width' = width + 4L in
  match code_tl with
  | CodeTlDelimited {delimited; code_tl} ->
    formatter |> Fmt.fmt "CodeTlDelimited "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "delimited=" |> fmt_delimited ~alt ~width:width' delimited
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code_tl=" |> fmt_code_tl ~alt ~width:width' code_tl
    |> fmt_rcurly ~alt ~width
  | CodeTlToken {token; code_tl} ->
    formatter |> Fmt.fmt "CodeTlToken "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "token=" |> Scan.Token.pp token
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code_tl=" |> fmt_code_tl ~alt ~width:width' code_tl
    |> fmt_rcurly ~alt ~width
  | CodeTlEpsilon ->
    formatter |> Fmt.fmt "CodeTlEpsilon"
and pp_code_tl code_tl formatter =
  fmt_code code_tl formatter

and fmt_code ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) code formatter =
  let width' = width + 4L in
  match code with
  | CodeDelimited {delimited; code_tl} ->
    formatter |> Fmt.fmt "CodeDelimited "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "delimited=" |> fmt_delimited ~alt ~width:width' delimited
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code_tl=" |> fmt_code_tl ~alt ~width:width' code_tl
    |> fmt_rcurly ~alt ~width
  | CodeToken {token; code_tl} ->
    formatter |> Fmt.fmt "CodeToken "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "token=" |> Scan.Token.pp token
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code_tl=" |> fmt_code_tl ~alt ~width:width' code_tl
    |> fmt_rcurly ~alt ~width
and pp_code code formatter =
  fmt_code code formatter

and fmt_prod_param_symbol ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod_param_symbol
  formatter =
  let width' = width + 4L in
  match prod_param_symbol with
  | ProdParamSymbolCident {cident} ->
    formatter |> Fmt.fmt "ProdParamSymbolCident "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "cident=" |> fmt_cident ~alt ~width:width' cident
    |> fmt_rcurly ~alt ~width
  | ProdParamSymbolAlias {alias} ->
    formatter |> Fmt.fmt "ProdParamSymbolAlias "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "alias=" |> Scan.Token.pp alias
    |> fmt_rcurly ~alt ~width
and pp_prod_param_symbol prod_param_symbol formatter =
  fmt_prod_param_symbol prod_param_symbol formatter

and fmt_prod_param ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod_param formatter =
  let width' = width + 4L in
  match prod_param with
  | ProdParamBinding {ident; colon; prod_param_symbol} ->
    formatter |> Fmt.fmt "ProdParam "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "ident=" |> fmt_ident ~alt ~width:width' ident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "colon=" |> Scan.Token.pp colon
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prod_param_symbol=" |> fmt_prod_param_symbol ~alt ~width:width' prod_param_symbol
    |> fmt_rcurly ~alt ~width  | ProdParam {prod_param_symbol} ->
    formatter |> Fmt.fmt "ProdParam "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod_param_symbol=" |> fmt_prod_param_symbol ~alt ~width:width' prod_param_symbol
    |> fmt_rcurly ~alt ~width
and pp_prod_param prod_param formatter =
  fmt_prod_param prod_param formatter

and fmt_prod_params_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod_params_tl formatter =
  let width' = width + 4L in
  match prod_params_tl with
  | ProdParamsTlProdParam {prod_param; prod_params_tl} ->
    formatter |> Fmt.fmt "ProdParamsTlProdParam "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod_param=" |> fmt_prod_param ~alt ~width:width' prod_param
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prod_params_tl=" |> fmt_prod_params_tl ~alt ~width:width' prod_params_tl
    |> fmt_rcurly ~alt ~width
  | ProdParamsTlEpsilon ->
    formatter |> Fmt.fmt "ProdParamsTlEpsilon"
and pp_prod_params_tl prod_params_tl formatter =
  fmt_prod_params_tl prod_params_tl formatter

and fmt_prod_params ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod_params formatter =
  let width' = width + 4L in
  match prod_params with
  | ProdParamsProdParam {prod_param; prod_params_tl} ->
    formatter |> Fmt.fmt "ProdParamsProdParam "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod_param=" |> fmt_prod_param ~alt ~width:width' prod_param
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prod_params_tl=" |> fmt_prod_params_tl ~alt ~width:width' prod_params_tl
    |> fmt_rcurly ~alt ~width
and pp_prod_params prod_params formatter =
  fmt_prod_params prod_params formatter

and fmt_prod_pattern ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod_pattern formatter =
  let width' = width + 4L in
  match prod_pattern with
  | ProdPatternParams {prod_params} ->
    formatter |> Fmt.fmt "ProdPatternParams "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod_params=" |> fmt_prod_params ~alt ~width:width' prod_params
    |> fmt_rcurly ~alt ~width
  | ProdPatternEpsilon {epsilon} ->
    formatter |> Fmt.fmt "ProdPatternEpsilon "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "epsilon=" |> Scan.Token.pp epsilon
    |> fmt_rcurly ~alt ~width
and pp_prod_pattern prod_pattern formatter =
  fmt_prod_pattern prod_pattern formatter

and fmt_prod ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prod formatter =
  let width' = width + 4L in
  match prod with
  | Prod {prod_pattern; prec_ref} ->
    formatter |> Fmt.fmt "Prod "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod_pattern=" |> fmt_prod_pattern ~alt ~width:width' prod_pattern
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prec_ref=" |> fmt_prec_ref ~alt ~width:width' prec_ref
    |> fmt_rcurly ~alt ~width
and pp_prod prod formatter =
  fmt_prod prod formatter

and fmt_prods_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prods_tl formatter =
  let width' = width + 4L in
  match prods_tl with
  | ProdsTlBarProd {bar; prod; prods_tl} ->
    formatter |> Fmt.fmt "ProdsTlBarProd "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "bar=" |> Scan.Token.pp bar
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prod=" |> fmt_prod ~alt ~width:width' prod
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prods_tl=" |> fmt_prods_tl ~alt ~width:width' prods_tl
    |> fmt_rcurly ~alt ~width
  | ProdsTlEpsilon ->
    formatter |> Fmt.fmt "ProdsTlEpsilon"
and pp_prods_tl prods_tl formatter =
  fmt_prods_tl prods_tl formatter

and fmt_prods ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) prods formatter =
  let width' = width + 4L in
  match prods with
  | ProdsBarProd {bar; prod; prods_tl} ->
    formatter |> Fmt.fmt "ProdsBarProd "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "bar=" |> Scan.Token.pp bar
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prod=" |> fmt_prod ~alt ~width:width' prod
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prods_tl=" |> fmt_prods_tl ~alt ~width:width' prods_tl
    |> fmt_rcurly ~alt ~width
  | ProdsProd {prod; prods_tl} ->
    formatter |> Fmt.fmt "ProdsProd "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prod=" |> fmt_prod ~alt ~width:width' prod
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prods_tl=" |> fmt_prods_tl ~alt ~width:width' prods_tl
    |> fmt_rcurly ~alt ~width
and pp_prods prods formatter =
  fmt_prods prods formatter

and fmt_reduction ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) reduction formatter =
  let width' = width + 4L in
  match reduction with
  | Reduction {prods; arrow; code} ->
    formatter |> Fmt.fmt "Reduction "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prods=" |> fmt_prods ~alt ~width:width' prods
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "arrow=" |> Scan.Token.pp arrow
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "code=" |> pp_code code
    |> fmt_rcurly ~alt ~width
and pp_reduction reduction formatter =
  fmt_reduction reduction formatter

and fmt_reductions_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) reductions_tl formatter =
  let width' = width + 4L in
  match reductions_tl with
  | ReductionsTlBarReduction {bar; reduction; reductions_tl} ->
    formatter |> Fmt.fmt "ReductionsTlBarReduction "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "bar=" |> Scan.Token.pp bar
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "reduction=" |> fmt_reduction ~alt ~width:width' reduction
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "reductions_tl=" |> fmt_reductions_tl ~alt ~width:width' reductions_tl
    |> fmt_rcurly ~alt ~width
  | ReductionsTlEpsilon ->
    formatter |> Fmt.fmt "ReductionsTlEpsilon"
and pp_reductions_tl reductions_tl formatter =
  fmt_reductions_tl reductions_tl formatter

and fmt_reductions ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) reductions formatter =
  let width' = width + 4L in
  match reductions with
  | ReductionsReduction {reduction; reductions_tl} ->
    formatter |> Fmt.fmt "ReductionsReduction "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "reduction=" |> fmt_reduction ~alt ~width:width' reduction
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "reductions_tl=" |> fmt_reductions_tl ~alt ~width:width' reductions_tl
    |> fmt_rcurly ~alt ~width
and pp_reductions reductions formatter =
  fmt_reductions reductions formatter

and fmt_nonterm_type ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) nonterm_type formatter =
  match nonterm_type with
  | NontermTypeNonterm {nonterm} ->
    formatter |> Fmt.fmt "NontermTypeNonterm "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "nonterm=" |> Scan.Token.pp nonterm
    |> fmt_rcurly ~alt ~width
  | NontermTypeStart {start} ->
    formatter |> Fmt.fmt "NontermTypeStart "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "start=" |> Scan.Token.pp start
    |> fmt_rcurly ~alt ~width
and pp_nonterm_type nonterm_type formatter =
  fmt_nonterm_type nonterm_type formatter

and fmt_nonterm ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) nonterm formatter =
  let width' = width + 4L in
  match nonterm with
  | NontermProds {nonterm_type; cident; prec_ref; cce; prods} ->
    formatter |> Fmt.fmt "NontermProds "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "nonterm_type=" |> fmt_nonterm_type ~alt ~width:width' nonterm_type
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "cident=" |> fmt_cident ~alt ~width:width' cident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prec_ref=" |> fmt_prec_ref ~alt ~width:width' prec_ref
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "cce=" |> Scan.Token.pp cce
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prods=" |> fmt_prods ~alt ~width:width' prods
    |> fmt_rcurly ~alt ~width
  | NontermReductions {nonterm_type; cident; of_type; prec_ref; cce; reductions} ->
    formatter |> Fmt.fmt "NontermReductions "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "nonterm_type=" |> fmt_nonterm_type ~alt ~width:width' nonterm_type
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "cident=" |> fmt_cident ~alt ~width:width' cident
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "of_type=" |> fmt_of_type ~alt ~width:width' of_type
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "prec_ref=" |> fmt_prec_ref ~alt ~width:width' prec_ref
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "cce=" |> Scan.Token.pp cce
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "reductions=" |> fmt_reductions ~alt ~width:width' reductions
    |> fmt_rcurly ~alt ~width
and pp_nonterm nonterm formatter =
  fmt_nonterm nonterm formatter

and fmt_stmt ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) stmt formatter =
  let width' = width + 4L in
  match stmt with
  | StmtPrec {prec} ->
    formatter |> Fmt.fmt "StmtPrec "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prec=" |> fmt_prec ~alt ~width:width' prec
    |> fmt_rcurly ~alt ~width
  | StmtToken {token} ->
    formatter |> Fmt.fmt "StmtToken "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "token=" |> fmt_token ~alt ~width:width' token
    |> fmt_rcurly ~alt ~width
  | StmtNonterm {nonterm} ->
    formatter |> Fmt.fmt "StmtNonterm "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "nonterm=" |> fmt_nonterm ~alt ~width:width' nonterm
    |> fmt_rcurly ~alt ~width
  | StmtCode {code} ->
    formatter |> Fmt.fmt "StmtCode "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "code=" |> fmt_code ~alt ~width:width' code
    |> fmt_rcurly ~alt ~width
and pp_stmt stmt formatter =
  fmt_stmt stmt formatter

and fmt_stmts_tl ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) stmts_tl formatter =
  let width' = width + 4L in
  match stmts_tl with
  | StmtsTl {line_delim; stmt; stmts_tl} ->
    formatter |> Fmt.fmt "StmtsTl "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "line_delim=" |> Scan.Token.pp line_delim
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "stmt=" |> fmt_stmt ~alt ~width:width' stmt
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "stmts_tl=" |> fmt_stmts_tl ~alt ~width:width' stmts_tl
    |> fmt_rcurly ~alt ~width
  | StmtsTlEpsilon ->
    formatter |> Fmt.fmt "StmtsTlEpsilon"
and pp_stmts_tl stmts_tl formatter =
  fmt_stmts_tl stmts_tl formatter

and fmt_stmts ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) stmts formatter =
  let width' = width + 4L in
  match stmts with
  | Stmts {stmt; stmts_tl} ->
    formatter |> Fmt.fmt "Stmts "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "stmt=" |> fmt_stmt ~alt ~width:width' stmt
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "stmts_tl=" |> fmt_stmts_tl ~alt ~width:width' stmts_tl
    |> fmt_rcurly ~alt ~width
and pp_stmts stmts formatter =
  fmt_stmts stmts formatter

and fmt_hocc ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) hocc formatter =
  let width' = width + 4L in
  match hocc with
  | Hocc {hocc; indent; stmts; dedent} ->
    formatter |> Fmt.fmt "Hocc "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "hocc=" |> Scan.Token.pp hocc
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "indent=" |> Scan.Token.pp indent
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "stmts=" |> fmt_stmts ~alt ~width:width' stmts
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "dedent=" |> Scan.Token.pp dedent
    |> fmt_rcurly ~alt ~width
and pp_hocc hocc formatter =
  fmt_hocc hocc formatter

and fmt_eoi ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) eoi formatter =
  match eoi with
  | Eoi {eoi} ->
    formatter |> Fmt.fmt "Eoi "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "eoi=" |> Scan.Token.pp eoi
    |> fmt_rcurly ~alt ~width
and pp_eoi eoi formatter =
  fmt_eoi eoi formatter

and fmt_matter ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) matter formatter =
  let width' = width + 4L in
  match matter with
  | Matter {token; matter} ->
    formatter |> Fmt.fmt "Matter "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "token=" |> Scan.Token.pp token
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "matter=" |> fmt_matter ~alt ~width:width' matter
    |> fmt_rcurly ~alt ~width
  | MatterEpsilon ->
    formatter |> Fmt.fmt "MatterEpsilon"
and pp_matter matter formatter =
  fmt_matter matter formatter

and fmt_hmh ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) hmh formatter =
  let width' = width + 4L in
  match hmh with
  | Hmh {prelude; hocc; postlude; eoi} ->
    formatter |> Fmt.fmt "Hmh "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prelude=" |> pp_matter prelude
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "hocc=" |> fmt_hocc ~alt ~width:width' hocc
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "postlude=" |> pp_matter postlude
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "eoi=" |> fmt_eoi ~alt ~width:width' eoi
    |> fmt_rcurly ~alt ~width
and pp_hmh hmh formatter =
  fmt_hmh hmh formatter

and fmt_hmhi ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) hmhi formatter =
  let width' = width + 4L in
  match hmhi with
  | Hmhi {prelude; hocc; postlude; eoi} ->
    formatter |> Fmt.fmt "Hmhi "
    |> fmt_lcurly ~alt ~width
    |> Fmt.fmt "prelude=" |> pp_matter prelude
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "hocc=" |> Scan.Token.pp hocc
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "postlude=" |> pp_matter postlude
    |> fmt_semi ~alt ~width
    |> Fmt.fmt "eoi=" |> fmt_eoi ~alt ~width:width' eoi
    |> fmt_rcurly ~alt ~width
and pp_hmhi hmhi formatter =
  fmt_hmhi hmhi formatter

(**************************************************************************************************)
(* Recursive descent parser. *)

let trace = false

type ctx = {
  scanner: Scan.t;
  errs: Error.t list;
}

let pp_ctx {scanner; errs} formatter =
  formatter
  |> Fmt.fmt "{scanner=" |> Scan.pp scanner
  |> Fmt.fmt "; errs=" |> (List.pp Error.pp) errs
  |> Fmt.fmt "}"

let rec next ?(all=false) spine ({scanner; errs} as ctx) =
  let scanner', tok = Scan.next scanner in
  let _ = if trace then
      File.Fmt.stderr
      |> Fmt.fmt "hocc (trace): next ~all:" |> Bool.pp all
      |> Fmt.fmt " " |> (List.pp String.pp) (List.rev spine)
      |> Fmt.fmt " " |> pp_ctx ctx |> ignore
  in
  let errs' = List.fold (Scan.Token.malformations tok) ~init:errs ~f:(fun accum mal ->
    Error.init_mal mal :: accum) in
  let ctx' = {scanner=scanner'; errs=errs'} in
  match all, tok with
  | _, HmcToken {atok=Tok_whitespace; _}
  | false, HmcToken {atok=(Tok_hash_comment|Tok_paren_comment _); _} -> begin
      let _ = if trace then
          File.Fmt.stderr |> Fmt.fmt " -> recurse (" |> Scan.Token.pp tok |> Fmt.fmt ")\n" |> ignore
      in
      next ~all spine ctx'
    end
  | _ -> begin
      let _ = if trace then
          File.Fmt.stderr |> Fmt.fmt " -> " |> Scan.Token.pp tok |> Fmt.fmt "\n" |> ignore in
      ctx', tok
    end

let err msg {scanner; errs} =
  {scanner; errs=(Error.init_scanner scanner msg) :: errs}

let err_token tok msg {scanner; errs} =
  {scanner; errs=(Error.init_token tok msg) :: errs}

let reduce ?(alt=true) spine ctx
  (fmt_t: ?alt:bool -> ?width:uns -> 'a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) t =
  let _ = if trace then
      File.Fmt.stderr |> Fmt.fmt "hocc (trace): reduce " |> (List.pp String.pp) (List.rev spine)
      |> Fmt.fmt " " |> pp_ctx ctx |> Fmt.fmt " " |> fmt_t ~alt t |> Fmt.fmt "\n" |> ignore
  in
  ctx, Some t

(* Map optional subtree result, passing the resulting ctx in to enable tail recursion. *)
let mapr ~child ~f spine ctx =
  let ctx', child_opt = child spine ctx in
  match child_opt with
  | None -> ctx', None
  | Some c -> f spine ctx' c

(* Map optional subtree result, without support for tail recursion. *)
let map ~child ~f
    ~(fmt_child: ?alt:bool -> ?width:uns -> 'a -> (module Fmt.Formatter) -> (module Fmt.Formatter))
    spine ctx =
  mapr ~child ~f:(fun spine ctx' c -> reduce spine ctx' fmt_child (f c)) spine ctx

let rec uident spine ctx =
  let spine = match trace with true -> "uident" :: spine | false -> [] in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_uident _; _} as uident ->
    reduce spine ctx' fmt_uident (Uident {uident})
  | _ -> err_token tok "Expected uident" ctx, None

and cident spine ctx =
  let spine = "cident" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_cident _; _} as cident ->
    reduce spine ctx' fmt_cident (Cident {cident})
  | _ -> err_token tok "Expected cident" ctx, None

and ident spine ctx =
  let spine = "ident" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_uident _; _} as uident ->
    reduce spine ctx' fmt_ident (IdentUident {uident=Uident {uident}})
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_cident _; _} as cident ->
    reduce spine ctx' fmt_ident (IdentCident {cident=Cident {cident}})
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_uscore; _} as uscore ->
    reduce spine ctx' fmt_ident (IdentUscore {uscore})
  | _ -> err_token tok "Expected ident" ctx, None

and precs_tl spine ctx =
  let spine = "precs_tl" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_comma; _} as comma ->
    mapr ~child:uident ~f:(fun spine ctx' uident ->
      map ~child:precs_tl ~f:(fun precs_tl ->
        PrecsTlCommaUident {comma; uident; precs_tl}
      ) ~fmt_child:fmt_precs_tl spine ctx'
    ) spine ctx'
  | _ -> reduce spine ctx fmt_precs_tl PrecsTlEpsilon

and precs spine ctx =
  let spine = "precs" :: spine in
  mapr ~child:uident ~f:(fun spine ctx' uident ->
    map ~child:precs_tl ~f:(fun precs_tl ->
      Precs {uident; precs_tl}
    ) ~fmt_child:fmt_precs spine ctx'
  ) spine ctx

and prec_rels spine ctx =
  let spine = "prec_rels" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_lt; _} as lt ->
    map ~child:precs ~f:(fun precs ->
      PrecRelsLtPrecs {lt; precs}
    ) ~fmt_child:fmt_prec_rels spine ctx'
  | _ -> reduce spine ctx fmt_prec_rels PrecRelsEpsilon

and prec_type spine ctx =
  let spine = "prec_type" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_neutral; _} as neutral ->
    reduce spine ctx' fmt_prec_type (PrecTypeNeutral {neutral})
  | HoccToken {atok=Scan.AbstractToken.Tok_left; _} as left ->
    reduce spine ctx' fmt_prec_type (PrecTypeLeft {left})
  | HoccToken {atok=Scan.AbstractToken.Tok_right; _} as right ->
    reduce spine ctx' fmt_prec_type (PrecTypeRight {right})
  | _ -> err_token tok "Expected precedence type" ctx, None

and prec spine ctx =
  let spine = "prec" :: spine in
  mapr ~child:prec_type ~f:(fun spine ctx' prec_type ->
    mapr ~child:uident ~f:(fun spine ctx' uident ->
      map ~child:prec_rels ~f:(fun prec_rels ->
        Prec {prec_type; uident; prec_rels}
      ) ~fmt_child:fmt_prec spine ctx'
    ) spine ctx'
  ) spine ctx

and of_type spine ctx =
  let spine = "of_type" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_of; _} as of_ ->
    let dot spine ctx = begin
      let spine = "dot" :: spine in
      let ctx', tok = next spine ctx in
      match tok with
      | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_dot; _} -> ctx', Some tok
      | _ -> err_token tok "Expected '.'" ctx, None
    end in
    mapr ~child:cident ~f:(fun spine ctx' type_module ->
      mapr ~child:dot ~f:(fun spine ctx' dot ->
        map ~child:uident ~f:(fun type_type ->
          OfType {of_; type_module; dot; type_type}
        ) ~fmt_child:fmt_of_type spine ctx'
      ) spine ctx'
    ) spine ctx'
  | _ -> err_token tok "Expected 'of'" ctx, None

and of_type0 spine ctx =
  let spine = "of_type0" :: spine in
  let _ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_of; _} ->
    map ~child:of_type ~f:(fun of_type ->
      OfType0OfType {of_type}
    ) ~fmt_child:fmt_of_type0 spine ctx
  | _ -> reduce spine ctx fmt_of_type0 OfType0Epsilon

and prec_ref spine ctx =
  let spine = "prec_ref" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_prec; _} as prec ->
    map ~child:uident ~f:(fun uident ->
      PrecRefPrecUident {prec; uident}
    ) ~fmt_child:fmt_prec_ref spine ctx'
  | _ -> reduce spine ctx fmt_prec_ref PrecRefEpsilon

and token_alias spine ctx =
  let spine = "token_alias" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_istring _; _} as alias ->
    reduce spine ctx' fmt_token_alias (TokenAlias {alias})
  | _ -> reduce spine ctx fmt_token_alias TokenAliasEpsilon

and token spine ctx =
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_token; _} as token ->
    mapr ~child:cident ~f:(fun spine ctx' cident ->
      mapr ~child:token_alias ~f:(fun spine ctx' token_alias ->
        mapr ~child:of_type0 ~f:(fun spine ctx' of_type0 ->
          map ~child:prec_ref ~f:(fun prec_ref ->
            Token {token; cident; token_alias; of_type0; prec_ref}
          ) ~fmt_child:fmt_token spine ctx'
        ) spine ctx'
      ) spine ctx'
    ) spine ctx'
  | _ -> err_token tok "Expected 'token' statement" ctx, None

and sep spine ctx =
  let spine = "sep" :: spine in
  let ctx', tok = next ~all:true spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_line_delim; _} as line_delim ->
    reduce spine ctx' fmt_sep (SepLineDelim {line_delim})
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_semi; _} as semi ->
    reduce spine ctx' fmt_sep (SepSemi {semi})
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_bar; _} as bar ->
    reduce spine ctx' fmt_sep (SepBar {bar})
  | _ -> ctx, None

and codes_tl spine ctx =
  let spine = "codes_tl" :: spine in
  let _ctx', tok = next ~all:true spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_line_delim; _}
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_semi; _}
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_bar; _} ->
    mapr ~child:sep ~f:(fun spine ctx' sep ->
      mapr ~child:code ~f:(fun spine ctx' code ->
        map ~child:codes_tl ~f:(fun codes_tl ->
          CodesTlSepCode {sep; code; codes_tl}
        ) ~fmt_child:fmt_codes_tl spine ctx'
      ) spine ctx'
    ) spine ctx
  | _ -> reduce spine ctx fmt_codes_tl CodesTlEpsilon

and codes spine ctx =
  let spine = "codes" :: spine in
  mapr ~child:code ~f:(fun spine ctx' code ->
    map ~child:codes_tl ~f:(fun codes_tl ->
      Codes {code; codes_tl}
    ) ~fmt_child:fmt_codes spine ctx'
  ) spine ctx

and codes0 spine ctx =
  let spine = "codes0" :: spine in
  let ctx', codes_opt = codes spine ctx in
  match codes_opt with
  | Some codes -> reduce spine ctx' fmt_codes0 (Codes0Codes {codes})
  | None -> reduce spine ctx fmt_codes0 Codes0Epsilon

and indent spine ctx =
  let spine = "indent" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_indent _; _} -> ctx', Some tok
  | _ -> err_token tok "Expected indent" ctx, None

and dedent ?all spine ctx =
  let spine = "dedent" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_dedent _; _} -> ctx', Some tok
  | _ -> err_token tok "Expected dedent" ctx, None

and rparen ?all spine ctx =
  let spine = "rparen" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_rparen; _} -> ctx', Some tok
  | _ -> err_token tok "Expected ')'" ctx, None

and rcapture ?all spine ctx =
  let spine = "rcapture" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_rcapture; _} -> ctx', Some tok
  | _ -> err_token tok "Expected '|)'" ctx, None

and rbrack ?all spine ctx =
  let spine = "rbrack" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_rbrack; _} -> ctx', Some tok
  | _ -> err_token tok "Expected ']'" ctx, None

and rarray ?all spine ctx =
  let spine = "rarray" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_rarray; _} -> ctx', Some tok
  | _ -> err_token tok "Expected '|]'" ctx, None

and rcurly ?all spine ctx =
  let spine = "rcurly" :: spine in
  let ctx', tok = next ?all spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_rcurly; _} -> ctx', Some tok
  | _ -> err_token tok "Expected '}'" ctx, None

and delimited spine ctx =
  let spine = "delimited" :: spine in
  let ctx', tok = next ~all:true spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_indent _; _} as indent ->
    mapr ~child:codes ~f:(fun spine ctx' codes ->
      map ~child:(dedent ~all:true) ~f:(fun dedent ->
        DelimitedBlock {indent; codes; dedent}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_lparen; _} as lparen ->
    mapr ~child:codes0 ~f:(fun spine ctx' codes0 ->
      map ~child:(rparen ~all:true) ~f:(fun rparen ->
        DelimitedParen {lparen; codes0; rparen}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_lcapture; _} as lcapture ->
    mapr ~child:codes0 ~f:(fun spine ctx' codes0 ->
      map ~child:(rcapture ~all:true) ~f:(fun rcapture ->
        DelimitedCapture {lcapture; codes0; rcapture}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_lbrack; _} as lbrack ->
    mapr ~child:codes0 ~f:(fun spine ctx' codes0 ->
      map ~child:(rbrack ~all:true) ~f:(fun rbrack ->
        DelimitedList {lbrack; codes0; rbrack}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_larray; _} as larray ->
    mapr ~child:codes0 ~f:(fun spine ctx' codes0 ->
      map ~child:(rarray ~all:true) ~f:(fun rarray ->
        DelimitedArray {larray; codes0; rarray}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_lcurly; _} as lcurly ->
    mapr ~child:codes0 ~f:(fun spine ctx' codes0 ->
      map ~child:(rcurly ~all:true) ~f:(fun rcurly ->
        DelimitedModule {lcurly; codes0; rcurly}
      ) ~fmt_child:fmt_delimited spine ctx'
    ) spine ctx'
  | _ -> err_token tok "Expected left delimiter" ctx, None

and code_tl spine ctx =
  let spine = "code_tl" :: spine in
  let ctx', tok = next ~all:true spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.(Tok_indent _|Tok_lparen|Tok_lcapture|Tok_lbrack
                                          |Tok_larray|Tok_lcurly); _} ->
    mapr ~child:delimited ~f:(fun spine ctx' delimited ->
      map ~child:code_tl ~f:(fun code_tl ->
        CodeTlDelimited {delimited; code_tl}
      ) ~fmt_child:fmt_code_tl spine ctx'
    ) spine ctx
  | HmcToken {atok=Hmc.Scan.AbstractToken.(Tok_dedent _|Tok_rparen|Tok_rcapture|Tok_rbrack
                                          |Tok_rarray|Tok_rcurly
                                          |Tok_line_delim|Tok_semi|Tok_bar); _} ->
    reduce spine ctx fmt_code_tl CodeTlEpsilon
  | HmcToken _ as token ->
    map ~child:code_tl ~f:(fun code_tl ->
      CodeTlToken {token; code_tl}
    ) ~fmt_child:fmt_code_tl spine ctx'
  | _ -> reduce spine ctx fmt_code_tl CodeTlEpsilon

and code spine ctx =
  let spine = "code" :: spine in
  let ctx', tok = next ~all:true spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.(Tok_indent _|Tok_lparen|Tok_lcapture|Tok_lbrack
                                          |Tok_larray|Tok_lcurly); _} ->
    mapr ~child:delimited ~f:(fun spine ctx' delimited ->
      map ~child:code_tl ~f:(fun code_tl ->
        CodeDelimited {delimited; code_tl}
      ) ~fmt_child:fmt_code spine ctx'
    ) spine ctx
  | HmcToken {atok=Hmc.Scan.AbstractToken.(Tok_dedent _|Tok_rparen|Tok_rcapture|Tok_rbrack
                                          |Tok_rarray|Tok_rcurly
                                          |Tok_line_delim|Tok_semi|Tok_bar); _} ->
    err_token tok "Expected Hemlock code" ctx, None
  | HmcToken _ as token ->
    map ~child:code_tl ~f:(fun code_tl ->
      CodeToken {token; code_tl}
    ) ~fmt_child:fmt_code spine ctx'
  | _ -> err_token tok "Expected Hemlock code" ctx, None

and prod_param_symbol spine ctx =
  let spine = "prod_param_symbol" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_cident _; _} as cident ->
    reduce spine ctx' fmt_prod_param_symbol (ProdParamSymbolCident {cident=Cident {cident}})
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_istring _; _} as alias ->
    reduce spine ctx' fmt_prod_param_symbol (ProdParamSymbolAlias {alias})
  | _ -> err_token tok "Expected production parameter symbol" ctx, None

and prod_param spine ctx =
  let spine = "prod_param" :: spine in
  let colon spine ctx = begin
    let spine = "colon" :: spine in
    let ctx', tok = next spine ctx in
    match tok with
    | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_colon; _} -> ctx', Some tok
    | _ -> err_token tok "Expected ':'" ctx, None
  end in
  let ctx', prod_param_binding_opt = mapr ~child:ident ~f:(fun spine ctx' ident ->
    mapr ~child:colon ~f:(fun spine ctx' colon ->
      map ~child:prod_param_symbol ~f:(fun prod_param_symbol ->
        ProdParamBinding {ident; colon; prod_param_symbol}
      ) ~fmt_child:fmt_prod_param spine ctx'
    ) spine ctx'
  ) spine ctx
  in
  match prod_param_binding_opt with
  | Some _ -> ctx', prod_param_binding_opt
  | None -> begin
      map ~child:prod_param_symbol ~f:(fun prod_param_symbol ->
        ProdParam {prod_param_symbol}
      ) ~fmt_child:fmt_prod_param spine ctx
    end

and prod_params_tl spine ctx =
  let spine = "prod_params_tl" :: spine in
  let ctx', prod_param_opt = prod_param spine ctx in
  match prod_param_opt with
  | Some prod_param ->
    map ~child:prod_params_tl ~f:(fun prod_params_tl ->
      ProdParamsTlProdParam {prod_param; prod_params_tl}
    ) ~fmt_child:fmt_prod_params_tl spine ctx'
  | None -> reduce spine ctx fmt_prod_params_tl ProdParamsTlEpsilon

and prod_params spine ctx =
  let spine = "prod_params" :: spine in
  mapr ~child:prod_param ~f:(fun spine ctx' prod_param ->
    map ~child:prod_params_tl ~f:(fun prod_params_tl ->
      ProdParamsProdParam {prod_param; prod_params_tl}
    ) ~fmt_child:fmt_prod_params spine ctx'
  ) spine ctx

and prod_pattern spine ctx =
  let spine = "prod_pattern" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_epsilon; _} as epsilon ->
    reduce spine ctx' fmt_prod_pattern (ProdPatternEpsilon {epsilon})
  | _ ->
    map ~child:prod_params ~f:(fun prod_params ->
      ProdPatternParams {prod_params}
    ) ~fmt_child:fmt_prod_pattern spine ctx

and prod spine ctx =
  let spine = "prod" :: spine in
  mapr ~child:prod_pattern ~f:(fun spine ctx' prod_pattern ->
    map ~child:prec_ref ~f:(fun prec_ref ->
      Prod {prod_pattern; prec_ref}
    ) ~fmt_child:fmt_prod spine ctx'
  ) spine ctx

and prods_tl spine ctx =
  let spine = "prods_tl" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_bar; _} as bar ->
    mapr ~child:prod ~f:(fun spine ctx' prod ->
      map ~child:prods_tl ~f:(fun prods_tl ->
        ProdsTlBarProd {bar; prod; prods_tl}
      ) ~fmt_child:fmt_prods_tl spine ctx'
    ) spine ctx'
  | _ -> reduce spine ctx fmt_prods_tl ProdsTlEpsilon

and prods spine ctx =
  let spine = "prods" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_bar; _} as bar ->
    mapr ~child:prod ~f:(fun spine ctx' prod ->
      map ~child:prods_tl ~f:(fun prods_tl ->
        ProdsBarProd {bar; prod; prods_tl}
      ) ~fmt_child:fmt_prods spine ctx'
    ) spine ctx'
  | _ ->
    mapr ~child:prod ~f:(fun spine ctx' prod ->
      map ~child:prods_tl ~f:(fun prods_tl ->
        ProdsProd {prod; prods_tl}
      ) ~fmt_child:fmt_prods spine ctx'
    ) spine ctx

and reduction spine ctx =
  let spine = "reduction" :: spine in
  let arrow spine ctx = begin
    let spine = "arrow" :: spine in
    let ctx', tok = next spine ctx in
    match tok with
    | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_arrow; _} -> ctx', Some tok
    | _ -> err_token tok "Expected '->'" ctx, None
  end in
  mapr ~child:prods ~f:(fun spine ctx' prods ->
    mapr ~child:arrow ~f:(fun spine ctx' arrow ->
      map ~child:code ~f:(fun code ->
        Reduction {prods; arrow; code}
      ) ~fmt_child:fmt_reduction spine ctx'
    ) spine ctx'
  ) spine ctx

and reductions_tl spine ctx =
  let spine = "reductions_tl" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_bar; _} as bar ->
    mapr ~child:reduction ~f:(fun spine ctx' reduction ->
      map ~child:reductions_tl ~f:(fun reductions_tl ->
        ReductionsTlBarReduction {bar; reduction; reductions_tl}
      ) ~fmt_child:fmt_reductions_tl spine ctx'
    ) spine ctx'
  | _ -> reduce spine ctx fmt_reductions_tl ReductionsTlEpsilon

and reductions spine ctx =
  let spine = "reductions" :: spine in
  mapr ~child:reduction ~f:(fun spine ctx' reduction ->
    map ~child:reductions_tl ~f:(fun reductions_tl ->
      ReductionsReduction {reduction; reductions_tl}
    ) ~fmt_child:fmt_reductions spine ctx'
  ) spine ctx

and nonterm_type spine ctx =
  let spine = "nonterm_type" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_nonterm; _} as nonterm ->
    reduce spine ctx' fmt_nonterm_type (NontermTypeNonterm {nonterm})
  | HoccToken {atok=Scan.AbstractToken.Tok_start; _} as start ->
    reduce spine ctx' fmt_nonterm_type (NontermTypeStart {start})
  | _ -> err_token tok "Expected 'nonterm'/'start'" ctx, None

and nonterm spine ctx =
  let spine = "nonterm" :: spine in
  let cce spine ctx = begin
    let spine = "cce" :: spine in
    let ctx', tok = next spine ctx in
    match tok with
    | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_colon_op "::="; _} -> ctx', Some tok
    | _ -> err_token tok "Expected '::='" ctx, None
  end in
  mapr ~child:nonterm_type ~f:(fun spine ctx' nonterm_type ->
    mapr ~child:cident ~f:(fun spine ctx' cident ->
      let _ctx'', tok = next spine ctx' in
      match tok with
      | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_of; _} ->
        mapr ~child:of_type ~f:(fun spine ctx' of_type ->
          mapr ~child:prec_ref ~f:(fun spine ctx' prec_ref ->
            mapr ~child:cce ~f:(fun spine ctx' cce ->
              map ~child:reductions ~f:(fun reductions ->
                NontermReductions {nonterm_type; cident; of_type; prec_ref; cce; reductions}
              ) ~fmt_child:fmt_nonterm spine ctx'
            ) spine ctx'
          ) spine ctx'
        ) spine ctx'
      | _ ->
        mapr ~child:prec_ref ~f:(fun spine ctx' prec_ref ->
          mapr ~child:cce ~f:(fun spine ctx' cce ->
            map ~child:prods ~f:(fun prods ->
              NontermProds {nonterm_type; cident; prec_ref; cce; prods}
            ) ~fmt_child:fmt_nonterm spine ctx'
          ) spine ctx'
        ) spine ctx'
    ) spine ctx'
  ) spine ctx

and stmt spine ctx =
  let spine = "stmt" :: spine in
  let _ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.(Tok_neutral|Tok_left|Tok_right); _} ->
    map ~child:prec ~f:(fun prec -> StmtPrec {prec}) ~fmt_child:fmt_stmt spine ctx
  | HoccToken {atok=Scan.AbstractToken.Tok_token; _} ->
    map ~child:token ~f:(fun token -> StmtToken {token}) ~fmt_child:fmt_stmt spine ctx
  | HoccToken {atok=Scan.AbstractToken.(Tok_nonterm|Tok_start); _} ->
    map ~child:nonterm ~f:(fun nonterm -> StmtNonterm {nonterm}) ~fmt_child:fmt_stmt spine ctx
  | _ -> map ~child:code ~f:(fun code -> StmtCode {code}) ~fmt_child:fmt_stmt spine ctx

and stmts_tl spine ctx =
  let spine = "stmts_tl" :: spine in
  let line_delim spine ctx = begin
    let spine = "line_delim" :: spine in
    let ctx', tok = next spine ctx in
    match tok with
    | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_line_delim; _} -> ctx', Some tok
    | _ -> err_token tok "Expected line delimiter" ctx, None
  end in
  let ctx', line_delim_opt = line_delim spine ctx in
  match line_delim_opt with
  | Some line_delim -> begin
      mapr ~child:stmt ~f:(fun spine ctx' stmt ->
        map ~child:stmts_tl ~f:(fun stmts_tl ->
          StmtsTl {line_delim; stmt; stmts_tl}
        ) ~fmt_child:fmt_stmts_tl spine ctx'
      ) spine ctx'
    end
  | None -> reduce spine ctx fmt_stmts_tl StmtsTlEpsilon

and stmts spine ctx =
  let spine = "stmts" :: spine in
  mapr ~child:stmt ~f:(fun spine ctx' stmt ->
    map ~child:stmts_tl ~f:(fun stmts_tl ->
      Stmts {stmt; stmts_tl}
    ) ~fmt_child:fmt_stmts spine ctx'
  ) spine ctx

and hocc spine ctx =
  let spine = "hocc" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HoccToken {atok=Scan.AbstractToken.Tok_hocc; _} as hocc ->
    mapr ~child:indent ~f:(fun spine ctx' indent ->
      mapr ~child:stmts ~f:(fun spine ctx' stmts ->
        map ~child:dedent ~f:(fun dedent ->
          Hocc {hocc; indent; stmts; dedent}
        ) ~fmt_child:fmt_hocc spine ctx'
      ) spine ctx'
    ) spine ctx'
  | _ -> err_token tok "Expected 'hocc' statement" ctx, None

and eoi spine ctx =
  let spine = "eoi" :: spine in
  let ctx', tok = next spine ctx in
  match tok with
  | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_end_of_input; _} as eoi ->
    reduce spine ctx' fmt_eoi (Eoi {eoi})
  | _ -> err "Unexpected token before eoi" ctx, None

and matter spine ctx =
  let spine = "matter" :: spine in
  let rec f ctx = begin
    let ctx', tok = next ~all:true spine ctx in
    match tok with
    | HoccToken _
    | HmcToken {atok=Hmc.Scan.AbstractToken.Tok_end_of_input; _} -> ctx, MatterEpsilon
    | HmcToken _ -> begin
        let ctx', matter_rchild = f ctx' in
        ctx', Matter {token=tok; matter=matter_rchild}
      end
  end in
  let ctx', matter = f ctx in
  reduce ~alt:false spine ctx' fmt_matter matter

and hmh scanner =
  let spine = ["hmh"] in
  let ctx = {scanner; errs=[]} in
  let ctx', hmh_opt =
    mapr ~child:matter ~f:(fun spine ctx' prelude ->
      mapr ~child:hocc ~f:(fun spine ctx' hocc ->
        mapr ~child:matter ~f:(fun spine ctx' postlude ->
          map ~child:eoi ~f:(fun eoi ->
            Hmh {prelude; hocc; postlude; eoi}
          ) ~fmt_child:fmt_hmh spine ctx'
        ) spine ctx'
      ) spine ctx'
    ) spine ctx
  in
  match ctx', hmh_opt with
  | {errs=(_ :: _); _}, _
  | _, None -> ctx'.scanner, Error ctx'.errs
  | {errs=[]; _}, Some hmh -> ctx'.scanner, Ok hmh

and hmhi scanner =
  let spine = ["hmhi"] in
  let hocc spine ctx = begin
    let spine = "hocc" :: spine in
    let ctx', tok = next spine ctx in
    match tok with
    | HoccToken {atok=Scan.AbstractToken.Tok_hocc; _} as hocc -> ctx', Some hocc
    | _ -> err "Expected 'hocc' keyword" ctx, None
  end in
  let ctx = {scanner; errs=[]} in
  let ctx', hmh_opt =
    mapr ~child:matter ~f:(fun spine ctx' prelude ->
      mapr ~child:hocc ~f:(fun spine ctx' hocc ->
        mapr ~child:matter ~f:(fun spine ctx' postlude ->
          map ~child:eoi ~f:(fun eoi ->
            Hmhi {prelude; hocc; postlude; eoi}
          ) ~fmt_child:fmt_hmhi spine ctx'
        ) spine ctx'
      ) spine ctx'
    ) spine ctx
  in
  match ctx', hmh_opt with
  | {errs=(_ :: _); _}, _
  | _, None -> ctx'.scanner, Error ctx'.errs
  | {errs=[]; _}, Some hmh -> ctx'.scanner, Ok hmh
