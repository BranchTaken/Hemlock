open Basis
open! Basis.Rudiments

type t = {
  algorithm: Conf.algorithm;
  precs: Precs.t;
  symbols: Symbols.t;
  prods: Prods.t;
  callbacks: Callbacks.t;
  states: State.t array;
}

let string_of_token token =
  Hmc.Source.Slice.to_string (Scan.Token.source token)

let string_of_alias_token token =
  match token with
  | Scan.Token.HmcToken (Tok_istring {istring=Constant istring; _}) -> istring
  | _ -> not_reached ()

let synthetic_name_of_start_name start_name =
  start_name ^ "'"

let precs_init io hmh =
  let rec fold_precs_tl io precs rels doms precs_tl = begin
    match precs_tl with
    | Parse.PrecsTlUident {uident; precs_tl} -> begin
        let name = string_of_token uident in
        let rels = match Set.mem name rels with
          | true -> begin
              let io =
                io.err
                |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                |> Fmt.fmt ": Redundant relation to precedence: " |> Fmt.fmt name |> Fmt.fmt "\n"
                |> Io.with_err io
              in
              Io.fatal io
            end
          | false -> Set.insert name rels
        in
        let doms = match Precs.prec_of_name name precs with
          | None -> begin
              let io =
                io.err
                |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                |> Fmt.fmt ": Relation to undefined precedence: " |> Fmt.fmt name |> Fmt.fmt "\n"
                |> Io.with_err io
              in
              Io.fatal io
            end
          | Some Prec.{index; doms=rel_doms; _} -> Ordset.insert index doms |> Ordset.union rel_doms
        in
        fold_precs_tl io precs rels doms precs_tl
      end
    | PrecsTlEpsilon -> io, doms
  end in
  let fold_precs io precs parse_precs = begin
    match parse_precs with
    | Parse.Precs {uident; precs_tl} -> begin
        let name = string_of_token uident in
        let rels = Set.singleton (module String) name in
        let doms = match Precs.prec_of_name name precs with
          | None -> begin
              let io =
                io.err
                |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                |> Fmt.fmt ": Relation to undefined precedence: " |> Fmt.fmt name |> Fmt.fmt "\n"
                |> Io.with_err io
              in
              Io.fatal io
            end
          | Some Prec.{index; doms; _} -> Ordset.insert index doms
        in
        fold_precs_tl io precs rels doms precs_tl
      end
  end in
  let fold_prec io precs parse_prec = begin
    match parse_prec with
    | Parse.Prec {prec_type; uident; prec_rels} -> begin
        let name = string_of_token uident in
        let assoc = match prec_type with
          | PrecTypeNeutral -> None
          | PrecTypeLeft -> Some Assoc.Left
          | PrecTypeRight -> Some Assoc.Right
        in
        let io, doms = match prec_rels with
          | PrecRelsPrecs {precs=parse_precs} -> fold_precs io precs parse_precs
          | PrecRelsEpsilon -> io, Ordset.empty (module Prec.Index)
        in
        let precs = match Precs.prec_index_of_name name precs with
          | Some _ -> begin
              let io =
                io.err
                |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                |> Fmt.fmt ": Redefined precedence: " |> Fmt.fmt name |> Fmt.fmt "\n"
                |> Io.with_err io
              in
              Io.fatal io
            end
          | None -> Precs.insert ~name ~assoc ~doms ~stmt:parse_prec precs
        in
        io, precs
      end
  end in
  let fold_stmt io precs stmt = begin
    match stmt with
    | Parse.StmtPrec {prec=parse_prec} -> fold_prec io precs parse_prec
    | _ -> io, precs
  end in
  let rec fold_stmts_tl io precs stmts_tl = begin
    match stmts_tl with
    | Parse.StmtsTl {stmt; stmts_tl; _} -> begin
        let io, precs = fold_stmt io precs stmt in
        fold_stmts_tl io precs stmts_tl
      end
    | StmtsTlEpsilon -> io, precs
  end in
  let fold_stmts io precs stmts = begin
    match stmts with
    | Parse.Stmts {stmt; stmts_tl} -> begin
        let io, precs = fold_stmt io precs stmt in
        fold_stmts_tl io precs stmts_tl
      end
  end in
  let io, precs = match hmh with Parse.Hmh {hocc_=Hocc {stmts; _}; _} ->
    fold_stmts io Precs.empty stmts
  in
  io, precs

let rec qualify_symbol_type symbol_type_qualifier symbol_type =
  match symbol_type_qualifier with
  | Parse.SymbolTypeQualifier {cident=CIDENT {token=cident}; symbol_type_qualifier_tl; _} -> begin
      qualify_symbol_type symbol_type_qualifier_tl
        (SymbolType.qualify (string_of_token cident) symbol_type)
    end
  | SymbolTypeQualifierEpsilon -> symbol_type

let tokens_init io precs hmh =
  let fold_token io precs symbols token = begin
    match token with
    | Parse.Token {cident=CIDENT {token=cident}; token_alias; symbol_type0; prec_ref; _}
      -> begin
          let name = string_of_token cident in
          let stype = match symbol_type0 with
            | SymbolType0SymbolType {symbol_type=SymbolType {
              symbol_type_qualifier; symbol_type; _}} -> begin
                SymbolType.explicit (string_of_token symbol_type)
                |> qualify_symbol_type symbol_type_qualifier
              end
            | SymbolType0Epsilon -> SymbolType.implicit
          in
          let prec = match prec_ref with
            | PrecRefUident {uident} -> begin
                let prec_name = string_of_token uident in
                match Precs.prec_of_name prec_name precs with
                | None -> begin
                    let io =
                      io.err
                      |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                      |> Fmt.fmt ": Undefined precedence: " |> Fmt.fmt prec_name |> Fmt.fmt "\n"
                      |> Io.with_err io
                    in
                    Io.fatal io
                  end
                | Some _ as prec -> prec
              end
            | PrecRefEpsilon -> None
          in
          let () = match Symbols.symbol_index_of_name name symbols with
            | None -> ()
            | Some _ -> begin
                let io =
                  io.err
                  |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source cident)
                  |> Fmt.fmt ": Redefined token: " |> Fmt.fmt name |> Fmt.fmt "\n"
                  |> Io.with_err io
                in
                Io.fatal io
              end
          in
          let alias = match token_alias with
            | TokenAlias {alias=ISTRING {token=a}} -> begin
                let alias_name = string_of_alias_token a in
                let () = match Symbols.symbol_index_of_alias alias_name symbols with
                  | None -> ()
                  | Some _ -> begin
                      let io =
                        io.err
                        |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source a)
                        |> Fmt.fmt ": Redefined token alias: " |> Fmt.fmt alias_name |> Fmt.fmt "\n"
                        |> Io.with_err io
                      in
                      Io.fatal io
                    end
                in
                Some alias_name
              end
            | TokenAliasEpsilon -> None
          in
          let symbols = Symbols.insert_token ~name ~stype ~prec ~stmt:(Some token) ~alias symbols in
          io, symbols
        end
  end in
  let fold_stmt io precs symbols stmt = begin
    match stmt with
    | Parse.StmtToken {token} -> fold_token io precs symbols token
    | _ -> io, symbols
  end in
  let rec fold_stmts_tl io precs symbols stmts_tl = begin
    match stmts_tl with
    | Parse.StmtsTl {stmt; stmts_tl; _} -> begin
        let io, symbols = fold_stmt io precs symbols stmt in
        fold_stmts_tl io precs symbols stmts_tl
      end
    | StmtsTlEpsilon -> io, symbols
  end in
  let fold_stmts io precs symbols stmts = begin
    match stmts with
    | Parse.Stmts {stmt; stmts_tl} -> begin
        let io, symbols = fold_stmt io precs symbols stmt in
        fold_stmts_tl io precs symbols stmts_tl
      end
  end in
  let io, symbols = match hmh with Parse.Hmh {hocc_=Hocc {stmts; _}; _} ->
    fold_stmts io precs Symbols.empty stmts
  in
  io, symbols

let symbol_infos_init io symbols hmh =
  let insert_symbol_info name stype name_token symbols = begin
    match Symbols.info_of_name name symbols with
    | Some _ -> begin
        let io =
          io.err
          |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source name_token)
          |> Fmt.fmt ": Redefined symbol: " |> Fmt.fmt name |> Fmt.fmt "\n"
          |> Io.with_err io
        in
        Io.fatal io
      end
    | None -> Symbols.insert_nonterm_info ~name ~stype symbols
  end in
  let fold_nonterm io symbols nonterm = begin
    let name, stype = match nonterm with
      | Parse.NontermProds {cident=CIDENT {token=nonterm_cident}; _} ->
        string_of_token nonterm_cident, SymbolType.implicit
      | NontermReductions {cident=CIDENT {token=nonterm_cident}; symbol_type=SymbolType {
        symbol_type_qualifier; symbol_type; _}; _} -> begin
          let name = string_of_token nonterm_cident in
          let stype = SymbolType.explicit (string_of_token symbol_type)
                      |> qualify_symbol_type symbol_type_qualifier in
          name, stype
        end
    in
    match nonterm with
    | NontermProds {nonterm_type; cident=CIDENT {token=cident}; _}
    | NontermReductions {nonterm_type; cident=CIDENT {token=cident}; _} -> begin
        let symbols = insert_symbol_info name stype cident symbols in
        let io, symbols = match nonterm_type with
          | NontermTypeNonterm -> io, symbols
          | NontermTypeStart -> begin
              (* Synthesize start symbol wrapper. *)
              let name' = synthetic_name_of_start_name name in
              let stype' = SymbolType.synthetic_wrapper stype in
              let symbols = insert_symbol_info name' stype' cident symbols in
              io, symbols
            end
        in
        io, symbols
      end
  end in
  let fold_stmt io symbols stmt = begin
    match stmt with
    | Parse.StmtNonterm {nonterm} -> fold_nonterm io symbols nonterm
    | _ -> io, symbols
  end in
  let rec fold_stmts_tl io symbols stmts_tl = begin
    match stmts_tl with
    | Parse.StmtsTl {stmt; stmts_tl; _} -> begin
        let io, symbols = fold_stmt io symbols stmt in
        fold_stmts_tl io symbols stmts_tl
      end
    | StmtsTlEpsilon -> io, symbols
  end in
  let fold_stmts io symbols stmts = begin
    match stmts with
    | Parse.Stmts {stmt; stmts_tl} -> begin
        let io, symbols = fold_stmt io symbols stmt in
        fold_stmts_tl io symbols stmts_tl
      end
  end in
  let io, symbols = match hmh with Parse.Hmh {hocc_=Hocc {stmts; _}; _} ->
    fold_stmts io symbols stmts
  in
  io, symbols

let symbols_init io precs symbols hmh =
  let fold_prod_param io symbols prod_params prod_param = begin
    match prod_param with
    | Parse.ProdParamBinding {prod_param_symbol; _}
    | ProdParamPattern {prod_param_symbol; _}
    | ProdParamFields {prod_param_symbol; _}
    | ProdParam {prod_param_symbol} -> begin
        let io, symbol_name, stype = match prod_param_symbol with
          | ProdParamSymbolCident {cident=CIDENT {token=cident}} -> begin
              let symbol_name = string_of_token cident in
              match Symbols.info_of_name symbol_name symbols with
              | None -> begin
                  let io =
                    io.err
                    |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source cident)
                    |> Fmt.fmt ": Undefined symbol: " |> Fmt.fmt symbol_name |> Fmt.fmt "\n"
                    |> Io.with_err io
                  in
                  Io.fatal io
                end
              | Some Symbols.{name; alias; stype; _} -> begin
                  let io = match alias with
                    | Some alias -> begin
                        io.log
                        |> Fmt.fmt "hocc: At "
                        |> Hmc.Source.Slice.pp (Scan.Token.source cident)
                        |> Fmt.fmt ": Unused token alias " |> String.pp alias |> Fmt.fmt " for "
                        |> Fmt.fmt symbol_name |> Fmt.fmt "\n"
                        |> Io.with_log io
                      end
                    | None -> io
                  in
                  io, name, stype
                end
            end
          | ProdParamSymbolAlias {alias=ISTRING {token=alias}} -> begin
              let alias_name = string_of_alias_token alias in
              match Symbols.info_of_alias alias_name symbols with
              | None -> begin
                  let io =
                    io.err
                    |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source alias)
                    |> Fmt.fmt ": Undefined alias: " |> String.pp alias_name |> Fmt.fmt "\n"
                    |> Io.with_err io
                  in
                  Io.fatal io
                end
              | Some Symbols.{name; stype; _} -> io, name, stype
            end
        in
        let pattern = match prod_param with
          | Parse.ProdParamBinding _
          | ProdParamPattern _
          | ProdParamFields _ -> begin
              let pattern_source =
                Parse.source_of_prod_param_binding_pattern prod_param
                |> Option.value_hlt in
              let pattern = pattern_source |> Hmc.Source.Slice.to_string in
              match SymbolType.is_explicit stype with
              | false -> begin
                  let io =
                    io.err
                    |> Fmt.fmt "hocc: At "
                    |> Hmc.Source.Slice.pp pattern_source
                    |> Fmt.fmt ": Cannot bind to empty symbol variant: "
                    |> Fmt.fmt (Hmc.Source.Slice.to_string pattern_source)
                    |> Fmt.fmt ":" |> Fmt.fmt symbol_name |> Fmt.fmt "\n"
                    |> Io.with_err io
                  in
                  Io.fatal io
                end
              | true -> Some pattern
            end
          | ProdParam _ -> None
        in
        let param =
          Callback.Param.init ~pattern ~symbol_name ~stype ~prod_param:(Some prod_param) in
        io, param :: prod_params
      end
  end in
  let rec fold_prod_params_tl io symbols prod_params
      prod_params_tl = begin
    match prod_params_tl with
    | Parse.ProdParamsTlProdParam {prod_param; prod_params_tl} -> begin
        let io, prod_params = fold_prod_param io symbols prod_params prod_param in
        fold_prod_params_tl io symbols prod_params prod_params_tl
      end
    | ProdParamsTlPrecRef {prec_ref} -> begin
        let io, rhs = Callback.Params.init io (Array.of_list_rev prod_params) in
        io, rhs, prec_ref
      end
  end in
  let fold_prod_pattern io symbols prod_pattern = begin
    match prod_pattern with
    | Parse.ProdPatternParams {prod_params=ProdParamsProdParam {prod_param; prod_params_tl}}
      -> begin
          let io, prod_params = fold_prod_param io symbols [] prod_param in
          fold_prod_params_tl io symbols prod_params prod_params_tl
        end
    | ProdPatternEpsilon {prec_ref; _} -> begin
        let io, rhs = Callback.Params.init io [||] in
        io, rhs, prec_ref
      end
  end in
  let fold_prod io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code nonterm_prods_set
      prod = begin
    match prod with
    | Parse.Prod {prod_pattern} -> begin
        let lhs_index = Symbols.(nonterm_info.index) in
        let io, rhs, prec_ref = fold_prod_pattern io symbols prod_pattern in
        let io = match code with
          | Some _ -> io
          | None -> begin
              (* Codeless productions have no use for parameter bindings. *)
              Callback.Params.fold ~init:io ~f:(fun io Callback.Param.{pattern; prod_param; _} ->
                match pattern with
                | Some pattern -> begin
                    let binding_pattern_source =
                      prod_param
                      |> Option.value_hlt
                      |> Parse.source_of_prod_param_binding_pattern
                      |> Option.value_hlt
                    in
                    io.log
                    |> Fmt.fmt "hocc: At "
                    |> Hmc.Source.Slice.pp binding_pattern_source
                    |> Fmt.fmt ": Unused parameter binding pattern: "
                    |> Fmt.fmt pattern
                    |> Fmt.fmt "\n"
                    |> Io.with_log io
                  end
                | None -> io
              ) rhs
            end
        in
        let rhs_indexes = Callback.Params.map ~f:(fun Callback.Param.{symbol_name; _} ->
          match Symbols.info_of_name_hlt symbol_name symbols with Symbols.{index; _} -> index
        ) rhs in
        let prec = match prec_ref with
          | PrecRefUident {uident} -> begin
              let prec_name = string_of_token uident in
              match Precs.prec_of_name prec_name precs with
              | None -> begin
                  let io =
                    io.err
                    |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                    |> Fmt.fmt ": Undefined precedence: " |> Fmt.fmt prec_name |> Fmt.fmt "\n"
                    |> Io.with_err io
                  in
                  Io.fatal io
                end
              | Some _ as prec -> begin
                  match nonterm_prec with
                  | Some _ -> begin
                      let io =
                        io.err
                        |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                        |> Fmt.fmt ": Precedence already specified by nonterm\n"
                        |> Io.with_err io
                      in
                      Io.fatal io
                    end
                  | None -> prec (* De-normalize; propagate precedence to prod. *)
                end
            end
          | PrecRefEpsilon -> nonterm_prec
        in
        let lhs = nonterm_info in
        let callback, callbacks = Callbacks.insert ~lhs ~rhs ~code callbacks in
        let prod, prods =
          Prods.insert ~lhs_index ~rhs_indexes ~prec ~stmt:(Some prod) ~callback prods in
        let nonterm_prods_set = Ordset.insert prod nonterm_prods_set in
        io, nonterm_prods_set, prods, callbacks, prod
      end
  end in
  let rec fold_prods_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code
      nonterm_prods_set prods_tl = begin
    match prods_tl with
    | Parse.ProdsTlProd {prod; prods_tl} -> begin
        let io, nonterm_prods_set, prods, callbacks, _prod =
          fold_prod io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code
            nonterm_prods_set prod in
        fold_prods_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code
          nonterm_prods_set prods_tl
      end
    | ProdsTlEpsilon -> io, nonterm_prods_set, prods, callbacks
  end in
  let fold_prods io precs symbols prods callbacks ~nonterm_info ~nonterm_prec parse_prods = begin
    match parse_prods with
    | Parse.ProdsProd {prod; prods_tl} -> begin
        let code = None in
        let nonterm_prods_set = Ordset.empty (module Prod) in
        let io, nonterm_prods_set, prods, callbacks, _prod =
          fold_prod io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code
            nonterm_prods_set prod in
        fold_prods_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec ~code
          nonterm_prods_set prods_tl
      end
  end in
  let fold_reduction io precs symbols prods callbacks ~nonterm_info ~nonterm_prec nonterm_prods_set
      reduction = begin
    match reduction with
    | Parse.Reduction {prods=parse_prods; code; _} -> begin
        (* Map one or more prods to copies of a single reduction callback, so that generated stack
         * pattern matching is per prod. *)
        match parse_prods with
        | ProdsProd {prod=parse_prod; prods_tl} -> begin
            let reduction_prods = Ordset.empty (module Prod) in
            let io, reduction_prods_merge, prods, callbacks, Prod.({callback={rhs; _}; _}) =
              fold_prod io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
                ~code:(Some code) reduction_prods parse_prod in
            let reduction_prods = Ordset.union reduction_prods_merge reduction_prods in
            let io, reduction_prods_merge, prods, callbacks =
              fold_prods_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
                ~code:(Some code) reduction_prods prods_tl in
            let reduction_prods = Ordset.union reduction_prods_merge reduction_prods in
            let bindings = Callback.Params.bindings rhs in
            let () = Ordset.iter ~f:(fun Prod.({callback={rhs=rhs1; _}; _} as prod1) ->
              let bindings1 = Callback.Params.bindings rhs1 in
              match Set.equal bindings bindings1 with
              | false -> begin
                  let Parse.(Prod {prod_pattern; _}) = Prod.(prod1.stmt) |> Option.value_hlt in
                  let pattern_source =
                    Parse.source_of_prod_pattern prod_pattern |> Option.value_hlt in
                  let io =
                    io.err
                    |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp pattern_source
                    |> Fmt.fmt ": Inconsistent production parametrization\n"
                    |> Io.with_err io
                  in
                  Io.fatal io
                end
              | true -> ()
            ) reduction_prods in
            let nonterm_prods_set = Ordset.union reduction_prods nonterm_prods_set in
            io, nonterm_prods_set, prods, callbacks
          end
      end
  end in
  let rec fold_reductions_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
      nonterm_prods_set reductions_tl = begin
    match reductions_tl with
    | Parse.ReductionsTlReduction {reduction; reductions_tl} -> begin
        let io, nonterm_prods_set, prods, callbacks =
          fold_reduction io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
            nonterm_prods_set reduction in
        fold_reductions_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
          nonterm_prods_set reductions_tl
      end
    | ReductionsTlEpsilon -> io, nonterm_prods_set, prods, callbacks
  end in
  let fold_reductions io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
      parse_reductions = begin
    match parse_reductions with
    | Parse.ReductionsReduction {reduction; reductions_tl} -> begin
        let nonterm_prods_set = Ordset.empty (module Prod) in
        let io, nonterm_prods_set, prods, callbacks =
          fold_reduction io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
            nonterm_prods_set reduction in
        fold_reductions_tl io precs symbols prods callbacks ~nonterm_info ~nonterm_prec
          nonterm_prods_set reductions_tl
      end
  end in
  let fold_nonterm io precs symbols prods callbacks nonterm = begin
    let start, name, prec = match nonterm with
      | Parse.NontermProds {nonterm_type; cident=CIDENT {token=cident}; prec_ref; _}
      | NontermReductions {nonterm_type; cident=CIDENT {token=cident}; prec_ref; _} -> begin
          let start = match nonterm_type with
            | NontermTypeNonterm -> false
            | NontermTypeStart -> true
          in
          let name = string_of_token cident in
          let prec = match prec_ref with
            | PrecRefUident {uident} -> begin
                let prec_name = string_of_token uident in
                match Precs.prec_of_name prec_name precs with
                | None -> begin
                    let io =
                      io.err
                      |> Fmt.fmt "hocc: At " |> Hmc.Source.Slice.pp (Scan.Token.source uident)
                      |> Fmt.fmt ": Undefined precedence: " |> Fmt.fmt prec_name |> Fmt.fmt "\n"
                      |> Io.with_err io
                    in
                    Io.fatal io
                  end
                | Some _ as prec -> prec
              end
            | PrecRefEpsilon -> None
          in
          start, name, prec
        end
    in
    let (Symbols.{index; stype; _} as nonterm_info) = Symbols.info_of_name_hlt name symbols in
    let nonterm_prec = prec in
    let io, nonterm_prods, prods, callbacks = match nonterm with
      | NontermProds {prods=parse_prods; _} ->
        fold_prods io precs symbols prods callbacks ~nonterm_info ~nonterm_prec parse_prods
      | NontermReductions {reductions; _} ->
        fold_reductions io precs symbols prods callbacks ~nonterm_info ~nonterm_prec reductions
    in
    let symbols =
      Symbols.insert_nonterm ~name ~prec ~stmt:(Some nonterm) ~start ~prods:nonterm_prods symbols in
    let io, symbols, prods, callbacks = match start with
      | false -> io, symbols, prods, callbacks
      | true -> begin
          (* Synthesize wrapper for start symbol. *)
          let name' = name ^ "'" in
          let Symbols.{index=index'; _} = Symbols.info_of_name_hlt name' symbols in
          let lhs = Symbols.{
            index=index';
            name=name';
            alias=None;
            stype=SymbolType.synthetic_wrapper stype;
          } in
          let Symbol.{index=pe_index; name=pe_name; stype=pe_stype; _} = Symbol.pseudo_end in
          let io, rhs = Callback.Params.init io [|
            Callback.Param.init ~pattern:(Some "start") ~symbol_name:name ~stype ~prod_param:None;
            Callback.Param.init ~pattern:None ~symbol_name:pe_name ~stype:pe_stype
              ~prod_param:None;
          |] in
          let callback, callbacks = Callbacks.insert ~lhs ~rhs ~code:None callbacks in
          let prod, prods = Prods.insert ~lhs_index:index' ~rhs_indexes:[|index; pe_index|]
            ~prec:None ~stmt:None ~callback prods in
          let nonterm_prods = Ordset.singleton (module Prod) prod in
          let symbols = Symbols.insert_nonterm ~name:name' ~prec:None ~stmt:None ~start
              ~prods:nonterm_prods symbols in
          io, symbols, prods, callbacks
        end
    in
    io, symbols, prods, callbacks
  end in
  let fold_stmt io precs symbols prods callbacks stmt = begin
    match stmt with
    | Parse.StmtNonterm {nonterm} -> fold_nonterm io precs symbols prods callbacks nonterm
    | _ -> io, symbols, prods, callbacks
  end in
  let rec fold_stmts_tl io precs symbols prods callbacks stmts_tl = begin
    match stmts_tl with
    | Parse.StmtsTl {stmt; stmts_tl; _} -> begin
        let io, symbols, prods, callbacks = fold_stmt io precs symbols prods callbacks stmt in
        fold_stmts_tl io precs symbols prods callbacks stmts_tl
      end
    | StmtsTlEpsilon -> io, symbols, prods, callbacks
  end in
  let fold_stmts io precs symbols prods callbacks stmts = begin
    match stmts with
    | Parse.Stmts {stmt; stmts_tl} -> begin
        let io, symbols, prods, callbacks = fold_stmt io precs symbols prods callbacks stmt in
        fold_stmts_tl io precs symbols prods callbacks stmts_tl
      end
  end in
  (* Compute first/follow sets for all symbols. *)
  let close_symbols symbols = begin
    (* Iterate to a fixed point, given a per prod folding function. *)
    let close_impl symbols ~fold_prod = begin
      let fold_prods symbols ~fold_prod symbol = begin
        Ordset.fold ~init:(symbols, symbol, false) ~f:(fun (symbols, symbol, merged) prod ->
          match fold_prod symbols symbol prod with
          | symbols', false -> symbols', symbol, merged
          | symbols', true ->
            symbols', Symbols.symbol_of_symbol_index Symbol.(symbol.index) symbols', true
        ) Symbol.(symbol.prods)
      end in
      let fold_nonterms symbols ~fold_prod = begin
        Symbols.nonterms_fold ~init:(symbols, false) ~f:(fun (symbols, merged) symbol ->
          match fold_prods symbols ~fold_prod symbol with
          | _, _, false -> symbols, merged
          | symbols', _, true -> symbols', true
        ) symbols
      end in
      let rec f symbols ~fold_prod = begin
        match fold_nonterms symbols ~fold_prod with
        | _, false -> symbols
        | symbols', true -> f symbols' ~fold_prod
      end in
      f symbols ~fold_prod
    end in
    let close_first symbols = begin
      let fold_prod symbols symbol prod = begin
        let lr0item = Lr0Item.init ~prod ~dot:0L in
        let lr1item = Lr1Item.init ~lr0item
            ~follow:(Ordset.singleton (module Symbol.Index) Symbol.(epsilon.index)) in
        let rhs_first = Lr1Item.first symbols lr1item in
        (* Merge the RHS's first set into symbol's first set. *)
        match Symbol.first_has_diff rhs_first symbol with
        | false -> symbols, false
        | true -> begin
            let symbol' = Symbol.first_union rhs_first symbol in
            let symbols' = Symbols.update_symbol symbol' symbols in
            symbols', true
          end
      end in
      close_impl symbols ~fold_prod
    end in
    let close_follow symbols = begin
      let fold_prod symbols symbol prod = begin
        match Array.length Prod.(prod.rhs_indexes) with
        | 0L -> symbols, false
        | _rhs_length -> begin
            Array.Slice.foldi (Array.Slice.init prod.rhs_indexes)
              ~init:(symbols, false) ~f:(fun i (symbols, merged) b_index ->
              (* A ::= αBβ *)
              let b = Symbols.symbol_of_symbol_index b_index symbols in
              let lr0item = Lr0Item.init ~prod ~dot:(succ i) in
              let lr1item = Lr1Item.init ~lr0item
                  ~follow:(Ordset.singleton (module Symbol.Index) Symbol.(epsilon.index)) in
              let first_beta = Lr1Item.first symbols lr1item in
              let first_beta_sans_epsilon = Ordset.remove Symbol.epsilon.index first_beta in
              (* Merge β's first set (sans "ε") into B's follow set. *)
              let symbols', b', merged' =
                match Symbol.follow_has_diff first_beta_sans_epsilon b with
                | false -> symbols, b, merged
                | true -> begin
                    let b' = Symbol.follow_union first_beta_sans_epsilon b in
                    let symbols' = Symbols.update_symbol b' symbols in
                    symbols', b', true
                  end
              in
              (* If β's first set contains "ε", merge A's follow set into B's follow set. *)
              let symbols', merged' = match Ordset.mem Symbol.epsilon.index first_beta &&
                                            Symbol.follow_has_diff Symbol.(symbol.follow) b' with
              | false -> symbols', merged'
              | true -> begin
                  let b' = Symbol.follow_union symbol.follow b' in
                  let symbols' = Symbols.update_symbol b' symbols in
                  symbols', true
                end
              in
              symbols', merged'
            )
          end
      end in
      close_impl symbols ~fold_prod
    end in
    symbols
    |> close_first
    |> close_follow
  end in
  (* Extract the non-terminal specifications from the AST. The end result will be:
   *
   * - `symbols`: Opaquely managed symbols collection
   * - `prods`/`callbacks` arrays: Each element encodes its own array offset
   *
   * Tokens have already been fully extracted into `symbols`, and basic info for non-terminals has
   * already been extracted into `symbols`; prod/callback indexes are incrementally assigned during
   * AST traversal. *)
  let callbacks = Callbacks.empty in
  let prods = Prods.empty in
  let io, symbols, prods, callbacks =
    match hmh with Parse.Hmh {hocc_=Hocc {stmts; _}; _} ->
      fold_stmts io precs symbols prods callbacks stmts
  in
  (* Close on symbols' first/follow sets. *)
  let symbols = close_symbols symbols in
  let nprecs = Precs.length precs in
  let ntokens = Symbols.tokens_length symbols in
  let nnonterms = Symbols.nonterms_length symbols in
  let nstarts = Symbols.nonterms_fold ~init:0L ~f:(fun nstarts (Symbol.{start; _} as symbol) ->
      match start && (not (Symbol.is_synthetic symbol)) with
      | false -> nstarts
      | true -> succ nstarts
    ) symbols in
  let nprods = Prods.length prods in
  let io =
    io.log
    |> Fmt.fmt "hocc: "
    |> Uns.pp nprecs |> Fmt.fmt " precedence"
    |> (fun formatter -> match nprecs with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")

    |> Fmt.fmt ", "
    |> Uns.pp ntokens |> Fmt.fmt " token"
    |> (fun formatter -> match ntokens with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")

    |> Fmt.fmt ", "
    |> Uns.pp nnonterms |> Fmt.fmt " non-terminal"
    |> (fun formatter -> match nnonterms with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> Fmt.fmt " (" |> Uns.pp nstarts
    |> Fmt.fmt " start"
    |> (fun formatter -> match nstarts with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> Fmt.fmt ")"

    |> Fmt.fmt ", "
    |> Uns.pp nprods |> Fmt.fmt " production"
    |> (fun formatter -> match nprods with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  let io = match nstarts with
    | 0L -> begin
        let io =
          io.err
          |> Fmt.fmt "hocc: Must specify at least one start symbol\n"
          |> Io.with_err io
        in
        Io.fatal io
      end
    | _ -> io
  in
  io, symbols, prods, callbacks

let compat_init algorithm ~resolve io symbols prods =
  let io, compat_string, compat = match algorithm with
    | Conf.Lr1 -> io, "lr1", StateNub.compat_lr1
    | Conf.Ielr1 -> io, "ielr1", StateNub.compat_ielr1 ~resolve symbols prods
    | Conf.Pgm1 -> io, "weak", StateNub.compat_pgm1
    | Conf.Lalr1 -> io, "lalr1", StateNub.compat_lalr1
  in
  let io =
    io.log
    |> Fmt.fmt "hocc: LR(1) item set compatibility: " |> Fmt.fmt compat_string |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  io, compat

let rec isocores_init algorithm ~resolve io precs symbols prods callbacks =
  (* Collect the LR(1) item set closures that comprise the initial work queue. There is one such
   * closure for each synthetic start symbol. *)
  let init symbols ~compat = begin
    let isocores, workq = Symbols.nonterms_fold
        ~init:(Isocores.init ~compat, Workq.empty)
        ~f:(fun ((isocores, workq) as accum) symbol ->
          match Symbol.is_synthetic symbol with
          | false -> accum
          | true -> begin
              assert (Uns.(=) (Ordset.length symbol.prods) 1L);
              let prod = Ordset.choose_hlt symbol.prods in (* There can be only one. ⚔ *)
              let dot = 0L in
              let lr0item = Lr0Item.init ~prod ~dot in
              let lr1item = Lr1Item.init ~lr0item ~follow:symbol.follow in
              let goto = Lr1Itemset.singleton lr1item in
              let kernel_attribs = KernelAttribs.empty in
              let gotonub = GotoNub.init ~isocores_sn_opt:None ~goto ~kernel_attribs in
              let index, isocores' = Isocores.insert symbols gotonub isocores in
              let workq' = Workq.push_back index workq in
              isocores', workq'
            end
        ) symbols in
    isocores, workq
  end in
  (* Iteratively process the work queue until no work remains. *)
  let rec close_gotonubs io symbols prods ~gotonub_of_statenub_goto isocores ~workq
      ~reported_isocores_length = begin
    match Workq.is_empty workq with
    | true -> io, isocores
    | false -> begin
        let index, workq' = Workq.pop workq in
        let statenub = Isocores.statenub index isocores in
        let io, isocores', workq', reported_isocores_length = Ordset.fold
            ~init:(io, isocores, workq', reported_isocores_length)
            ~f:(fun (io, isocores, workq, reported_isocores_length) symbol_index ->
              let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
              let goto = StateNub.goto symbol statenub in
              let gotonub = gotonub_of_statenub_goto statenub goto in
              let io, isocores, workq = match Isocores.get gotonub isocores with
                | None -> begin
                    let io =
                      io.log
                      |> Fmt.fmt (match (Isocores.mem (Lr1Itemset.core goto) isocores) with
                        | false -> "+"
                        | true -> "^"
                      )
                      |> Io.with_log io
                    in
                    let index, isocores' = Isocores.insert symbols gotonub isocores in
                    let workq' = Workq.push_back index workq in
                    io, isocores', workq'
                  end
                | Some merge_index -> begin
                    match Isocores.merge symbols gotonub merge_index isocores with
                    | false, _ -> io, isocores, workq
                    | true, isocores' -> begin
                        let io = io.log |> Fmt.fmt "." |> Io.with_log io in
                        let workq' = match Workq.mem merge_index workq with
                          | true -> workq
                          | false -> Workq.push merge_index workq
                        in
                        io, isocores', workq'
                      end
                  end
              in
              let isocores_length = Isocores.length isocores in
              let io, reported_isocores_length =
                match (isocores_length % 100L) = 0L && isocores_length > reported_isocores_length
                with
                | false -> io, reported_isocores_length
                | true -> begin
                    let io =
                      io.log
                      |> Fmt.fmt "["
                      |> Uns.pp (Workq.length workq)
                      |> Fmt.fmt "/"
                      |> Uns.pp isocores_length
                      |> Fmt.fmt "]"
                      |> Io.with_log io
                    in
                    io, isocores_length
                  end
              in
              io, isocores, workq, reported_isocores_length
            ) (StateNub.next statenub)
        in
        close_gotonubs io symbols prods ~gotonub_of_statenub_goto isocores' ~workq:workq'
          ~reported_isocores_length
      end
  end in
  let io, gotonub_of_statenub_goto =
    match algorithm with
    | Conf.Ielr1 -> begin
        let io =
          io.log
          |> Fmt.fmt "hocc: Generating LALR(1) specification as IELR(1) prerequisite\n"
          |> Io.with_log io
        in
        let io, lalr1_isocores, lalr1_states =
          init_inner Conf.Lalr1 ~resolve:false io precs symbols prods callbacks in
        Ielr1.gen_gotonub_of_statenub_goto ~resolve io symbols prods lalr1_isocores lalr1_states
      end
    | _ -> begin
        io,
        (fun _statenub goto ->
            GotoNub.init ~isocores_sn_opt:None ~goto ~kernel_attribs:KernelAttribs.empty)
      end
  in
  let io, compat = compat_init algorithm ~resolve io symbols prods in
  let isocores, workq = init symbols ~compat in
  let io =
    io.log
    |> Fmt.fmt "hocc: Generating LR(1) item set closures (+^.=add/split/merge)"
    |> Io.with_log io
  in
  let io, isocores =
    close_gotonubs io symbols prods ~gotonub_of_statenub_goto isocores ~workq
      ~reported_isocores_length:0L in
  let io = io.log |> Fmt.fmt "\n" |> Io.with_log io in
  io, isocores, gotonub_of_statenub_goto

and states_init io ~resolve symbols prods isocores ~gotonub_of_statenub_goto =
  let nstates = Isocores.length isocores in
  let io =
    io.log
    |> Fmt.fmt "hocc: Generating " |> Uns.pp nstates |> Fmt.fmt " LR(1) state"
    |> (fun formatter -> match nstates with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  let states =
    Isocores.fold ~init:(Ordset.empty (module State))
      ~f:(fun states lr1itemsetclosure ->
        let state = State.init ~resolve symbols prods isocores ~gotonub_of_statenub_goto
            lr1itemsetclosure in
        Ordset.insert state states
      ) isocores
    |> Ordset.to_array
  in
  let conflicts, conflict_states =
    Array.fold ~init:(0L, 0L) ~f:(fun (conflicts, conflict_states) state ->
      match State.conflicts ~filter_pseudo_end:false state with
      | 0L -> conflicts, conflict_states
      | x -> conflicts + x, succ conflict_states
    ) states
  in
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp conflicts
    |> (fun formatter ->
      match resolve with
      | false -> formatter
      | true -> formatter |> Fmt.fmt " unresolvable"
    )
    |> Fmt.fmt " conflict"
    |> (fun formatter -> match conflicts with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> Fmt.fmt " in " |> Uns.pp conflict_states
    |> Fmt.fmt " state"
    |> (fun formatter -> match conflict_states with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
    |> (fun formatter -> match conflicts = 0L with
      | true -> formatter
      | false -> begin
          let pseudo_end_conflicts =
            match Array.reduce ~f:Uns.(+)
              (Array.map ~f:(fun state -> Bool.to_uns (State.has_pseudo_end_conflict state)) states)
            with
            | None -> 0L
            | Some conflicts -> conflicts
          in
          let sr_conflicts =
            match Array.reduce ~f:Uns.(+)
              (Array.map ~f:(fun state -> State.sr_conflicts state) states) with
            | None -> 0L
            | Some conflicts -> conflicts
          in
          let rr_conflicts =
            match Array.reduce ~f:Uns.(+)
              (Array.map ~f:(fun state -> State.rr_conflicts state) states) with
            | None -> 0L
            | Some conflicts -> conflicts
          in
          formatter
          |> Fmt.fmt " (" |> Uns.pp pseudo_end_conflicts |> Fmt.fmt " ⊥, "
          |> Uns.pp sr_conflicts |> Fmt.fmt " shift-reduce, "
          |> Uns.pp rr_conflicts |> Fmt.fmt " reduce-reduce)"
          |> (fun formatter ->
            match resolve with
            | true -> formatter
            | false -> formatter |> Fmt.fmt " (conflict resolution disabled)"
          )
        end
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  io, states

and log_unused io precs symbols prods states =
  let rec mark_prec ~precs_used prec = begin
    match prec with
    | None -> precs_used
    | Some prec -> Set.insert Prec.(prec.index) precs_used
  end
  and mark_symbol ~precs_used ~tokens_used ~nonterms_used
      (Symbol.{index; prec; _} as symbol) = begin
    let precs_used = mark_prec ~precs_used prec in
    let tokens_used, nonterms_used = match Symbol.is_token symbol with
      | true -> Set.insert index tokens_used, nonterms_used
      | false -> tokens_used, Set.insert index nonterms_used
    in
    precs_used, tokens_used, nonterms_used
  end
  and mark_prod symbols ~precs_used ~tokens_used ~nonterms_used ~prods_used prod = begin
    let precs_used = mark_prec ~precs_used Prod.(prod.prec) in
    let precs_used, tokens_used, nonterms_used = mark_symbol ~precs_used ~tokens_used ~nonterms_used
        (Symbols.symbol_of_symbol_index Prod.(prod.lhs_index) symbols) in
    let precs_used, tokens_used, nonterms_used = Array.fold
        ~init:(precs_used, tokens_used, nonterms_used)
        ~f:(fun (precs_used, tokens_used, nonterms_used) rhs_index ->
          mark_symbol ~precs_used ~tokens_used ~nonterms_used
            (Symbols.symbol_of_symbol_index rhs_index symbols)
        ) prod.rhs_indexes
    in
    let prods_used = Set.insert Prod.(prod.index) prods_used in
    precs_used, tokens_used, nonterms_used, prods_used
  end
  and mark_state symbols prods ~precs_used ~tokens_used ~nonterms_used ~prods_used state = begin
    Ordmap.fold ~init:(precs_used, tokens_used, nonterms_used, prods_used)
      ~f:(fun (precs_used, tokens_used, nonterms_used, prods_used) (symbol_index, actions) ->

        let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
        let precs_used, tokens_used, nonterms_used =
          mark_symbol ~precs_used ~tokens_used ~nonterms_used symbol in
        Ordset.fold ~init:(precs_used, tokens_used, nonterms_used, prods_used)
          ~f:(fun (precs_used, tokens_used, nonterms_used, prods_used) action ->
            let open State.Action in
            match action with
            | ShiftPrefix _
            | ShiftAccept _ -> precs_used, tokens_used, nonterms_used, prods_used
            | Reduce prod_index -> begin
                let prod = Prods.prod_of_prod_index prod_index prods in
                mark_prod symbols ~precs_used ~tokens_used ~nonterms_used ~prods_used prod
              end
          ) actions
      ) State.(state.actions)
  end
  and mark_states symbols prods states = begin
    let precs_used = Set.empty (module Prec.Index) in
    let tokens_used = Set.singleton (module Symbol.Index) Symbol.epsilon.index in
    let nonterms_used = Set.empty (module Symbol.Index) in
    let prods_used = Set.empty (module Prod.Index) in
    Array.fold ~init:(precs_used, tokens_used, nonterms_used, prods_used)
      ~f:(fun (precs_used, tokens_used, nonterms_used, prods_used) state ->
        mark_state symbols prods ~precs_used ~tokens_used ~nonterms_used ~prods_used state
      ) states
  end in
  let io =
    io.log |> Fmt.fmt "hocc: Searching for unused precedences/tokens/non-terminals/productions\n"
    |> Io.with_log io
  in
  let precs_used, tokens_used, nonterms_used, prods_used = mark_states symbols prods states in
  let precs_nunused = (Precs.length precs) - (Set.length precs_used) in
  let tokens_nunused = (Symbols.tokens_length symbols) - (Set.length tokens_used) in
  let nonterms_nunused = (Symbols.nonterms_length symbols) - (Set.length nonterms_used) in
  let prods_nunused = (Prods.length prods) - (Set.length prods_used) in
  let io = match precs_nunused with
    | 0L -> io
    | _ -> begin
        io.log
        |> Fmt.fmt "hocc: " |> Uns.pp precs_nunused |> Fmt.fmt " unused precedence"
        |> (fun formatter ->
          match precs_nunused with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
        )
        |> Fmt.fmt ":\n"
        |> (fun formatter ->
          Precs.fold ~init:formatter ~f:(fun formatter prec ->
            match Set.mem Prec.(prec.index) precs_used with
            | true -> formatter
            | false -> formatter |> Fmt.fmt "hocc:" |> Prec.src_fmt prec
          ) precs
        )
        |> Io.with_log io
      end
  in
  let io = match tokens_nunused with
    | 0L -> io
    | _ -> begin
        io.log
        |> Fmt.fmt "hocc: " |> Uns.pp tokens_nunused |> Fmt.fmt " unused token"
        |> (fun formatter ->
          match tokens_nunused with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
        )
        |> Fmt.fmt ":\n"
        |> (fun formatter ->
          Symbols.tokens_fold ~init:formatter ~f:(fun formatter token ->
            match Set.mem Symbol.(token.index) tokens_used with
            | true -> formatter
            | false -> formatter |> Fmt.fmt "hocc:" |> Symbols.src_fmt token symbols
          ) symbols
        )
        |> Io.with_log io
      end
  in
  let io = match nonterms_nunused with
    | 0L -> io
    | _ -> begin
        io.log
        |> Fmt.fmt "hocc: " |> Uns.pp nonterms_nunused |> Fmt.fmt " unused non-terminal"
        |> (fun formatter ->
          match nonterms_nunused with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
        )
        |> Fmt.fmt ":\n"
        |> (fun formatter ->
          Symbols.nonterms_fold ~init:formatter ~f:(fun formatter Symbol.{index; name; prec; _} ->
            match Set.mem index nonterms_used with
            | true -> formatter
            | false -> begin
                formatter
                |> Fmt.fmt "hocc:    nonterm "
                |> Fmt.fmt name
                |> (fun formatter ->
                  match prec with
                  | None -> formatter
                  | Some {name; _} ->
                    formatter |> Fmt.fmt " prec " |> Fmt.fmt name
                )
                |> Fmt.fmt "\n"
              end
          ) symbols
        )
        |> Io.with_log io
      end
  in
  let io = match prods_nunused with
    | 0L -> io
    | _ -> begin
        io.log
        |> Fmt.fmt "hocc: " |> Uns.pp prods_nunused |> Fmt.fmt " unused production"
        |> (fun formatter ->
          match prods_nunused with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
        )
        |> Fmt.fmt ":\n"
        |> (fun formatter ->
          Prods.fold ~init:formatter
            ~f:(fun formatter Prod.{index; lhs_index; rhs_indexes; prec; _} ->
              match Set.mem index prods_used with
              | true -> formatter
              | false -> begin
                  let lhs_symbol = Symbols.symbol_of_symbol_index lhs_index symbols in
                  formatter
                  |> Fmt.fmt "hocc:    "
                  |> Fmt.fmt lhs_symbol.name
                  |> Fmt.fmt " ::="
                  |> (fun formatter ->
                    match Array.length rhs_indexes with
                    | 0L -> formatter |> Fmt.fmt " epsilon"
                    | _ -> begin
                        Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
                          let rhs_symbol = Symbols.symbol_of_symbol_index rhs_index symbols in
                          formatter
                          |> Fmt.fmt " "
                          |> (fun formatter ->
                            match rhs_symbol.alias with
                            | None -> formatter |> Fmt.fmt rhs_symbol.name
                            | Some alias -> formatter |> String.pp alias
                          )
                        ) rhs_indexes
                      end
                  )
                  |> (fun formatter ->
                    match prec with
                    | None -> formatter
                    | Some {name; _} -> formatter |> Fmt.fmt " prec " |> Fmt.fmt name
                  )
                  |> Fmt.fmt "\n"
                end
            ) prods
        )
        |> Io.with_log io
      end
  in
  io

and hmh_extract io hmh =
  let io, precs = precs_init io hmh in
  let io, symbols = tokens_init io precs hmh in
  let io, symbols = symbol_infos_init io symbols hmh in
  let io, symbols, prods, callbacks = symbols_init io precs symbols hmh in
  io, precs, symbols, prods, callbacks

and gc_states io isocores states =
  let state_indexes_reachable states = begin
    let isucc_state_indexes_of_state_index states state_index = begin
      let state = Array.get state_index states in
      let shift_isucc_state_indexes = Ordmap.fold ~init:(Ordset.empty (module State.Index))
        ~f:(fun isucc_state_indexes (_symbol_index, actions) ->
          Ordset.fold ~init:isucc_state_indexes ~f:(fun isucc_state_indexes action ->
            let open State.Action in
            match action with
            | ShiftPrefix isucc_state_index
            | ShiftAccept isucc_state_index -> Ordset.insert isucc_state_index isucc_state_indexes
            | Reduce _ -> isucc_state_indexes
          ) actions
        ) State.(state.actions) in
      Ordmap.fold ~init:shift_isucc_state_indexes
        ~f:(fun isucc_state_indexes (_symbol_index, goto) ->
          Ordset.insert goto isucc_state_indexes
        ) State.(state.gotos)
    end in
    let starts = Array.fold ~init:(Ordset.empty (module State.Index)) ~f:(fun reachable state ->
      match State.is_start state with
      | false -> reachable
      | true -> Ordset.insert (State.index state) reachable
    ) states in
    let rec trace states reachable state_index = begin
      Ordset.fold ~init:reachable ~f:(fun reachable isucc_state_index ->
        let isucc_state = Array.get isucc_state_index states in
        let isucc_state_index = State.index isucc_state in
        match Ordset.mem isucc_state_index reachable with
        | true -> reachable
        | false -> trace states (Ordset.insert isucc_state_index reachable) isucc_state_index
      ) (isucc_state_indexes_of_state_index states state_index)
    end in
    Ordset.fold ~init:starts ~f:(fun reachable state_index ->
      trace states reachable state_index
    ) starts
  end in
  let reachable_state_indexes = state_indexes_reachable states in
  let unreachable_state_indexes = Array.fold ~init:(Ordset.empty (module State.Index))
    ~f:(fun unreachable state ->
      let index = State.index state in
      match Ordset.mem index reachable_state_indexes with
      | true -> unreachable
      | false -> Ordset.insert index unreachable
    ) states in
  let nreachable = Ordset.length reachable_state_indexes in
  let nunreachable = Ordset.length unreachable_state_indexes in
  assert (Uns.(nreachable + nunreachable = Array.length states));
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp nunreachable |> Fmt.fmt " unreachable state"
    |> (fun formatter ->
      match nunreachable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  match nunreachable with
  | 0L -> io, isocores, states
  | _ -> begin
      let io =
        io.log
        |> Fmt.fmt "hocc: Reindexing " |> Uns.pp nreachable |> Fmt.fmt " LR(1) state"
        |> (fun formatter -> match nreachable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
        |> Fmt.fmt "\n"
        |> Io.with_log io
      in
      (* Create a map of pre-GC state indexes to post-GC state indexes. *)
      let state_index_map = Ordset.foldi ~init:(Ordmap.empty (module State.Index))
        ~f:(fun i state_index_map state_index ->
          Ordmap.insert_hlt ~k:state_index ~v:i state_index_map
        ) reachable_state_indexes in
      (* Create a new set of reindexed isocores. *)
      let reindexed_isocores =
        Ordset.fold ~init:isocores ~f:(fun remaining_isocores index ->
          Isocores.remove_hlt index remaining_isocores
        ) unreachable_state_indexes
        |> Isocores.reindex state_index_map in
      (* Create a new set of reindexed states. *)
      let reindexed_states =
        Array.fold ~init:(Ordset.empty (module State)) ~f:(fun reindexed_states state ->
          let state_index = State.index state in
          match Ordset.mem state_index reachable_state_indexes with
          | false -> reindexed_states
          | true -> begin
              let reindexed_state = State.reindex state_index_map state in
              Ordset.insert reindexed_state reindexed_states
            end
        ) states
        |> Ordset.to_array in
      io, reindexed_isocores, reindexed_states
    end

and remerge_states io symbols isocores states =
  let rec work io isocores states remergeables = begin
    let progress, remergeables =
      (* Initialize the work list with indices of all states in non-singleton isocore sets. *)
      Isocores.fold_isocore_sets ~init:[] ~f:(fun state_indexes isocore_set ->
        match Ordset.length isocore_set with
        | 0L -> not_reached ()
        | 1L -> state_indexes
        | _ -> Ordset.fold ~init:state_indexes ~f:(fun workq index -> index :: workq ) isocore_set
      ) isocores
      |> List.fold ~init:(false, remergeables)
        ~f:(fun (progress, remergeables) state_index ->
          let State.{statenub; _} as state = Array.get state_index states in
          assert Uns.(State.index state = state_index);
          let core = Lr1Itemset.core StateNub.(statenub.lr1itemsetclosure).kernel in
          let isocore_set = Isocores.get_isocore_set_hlt core isocores in
          Ordset.fold ~init:(progress, remergeables)
            ~f:(fun (progress, remergeables) iso_index ->
              (* Eliminate redundant/self pairs via `<=`. *)
              match State.Index.(iso_index <= state_index) with
              | true -> progress, remergeables
              | false -> begin
                  let iso_state = Array.get iso_index states in
                  let index_map = Remergeables.index_map remergeables in
                  match State.remergeable index_map iso_state state with
                  | false -> progress, remergeables
                  | true -> begin
                      let iso_statenub = iso_state.statenub in
                      let statenub = state.statenub in
                      match Remergeables.mem iso_statenub remergeables &&
                            Remergeables.mem statenub remergeables with
                      | true -> progress, remergeables
                      | false -> true, Remergeables.insert iso_statenub statenub remergeables
                    end
                end
            ) isocore_set
        )
    in
    (* Iterate until there is no remergability progress. *)
    match progress with
    | false -> io, remergeables
    | true -> work io isocores states remergeables
  end in
  let io, remergeables = work io isocores states Remergeables.empty in
  let remergeable_index_map = Remergeables.index_map remergeables in
  let nremergeable = Ordmap.length remergeable_index_map in
  let io =
    io.log
    |> Fmt.fmt "hocc: " |> Uns.pp nremergeable |> Fmt.fmt " remergeable state"
    |> (fun formatter ->
      match nremergeable with 1L -> formatter | _ -> formatter |> Fmt.fmt "s"
    )
    |> Fmt.fmt "\n"
    |> Io.with_log io
  in
  match nremergeable with
  | 0L -> io, isocores, states
  | _ -> begin
      let remaining_state_indexes = Range.Uns.fold (0L =:< Array.length states)
        ~init:(Ordset.empty (module State.Index))
        ~f:(fun reachable_state_indexes i ->
          match Ordmap.mem i remergeable_index_map with
          | true -> reachable_state_indexes
          | false -> Ordset.insert i reachable_state_indexes
        ) in
      let nremaining = Ordset.length remaining_state_indexes in
      let io =
        io.log
        |> Fmt.fmt "hocc: Reindexing " |> Uns.pp nremaining |> Fmt.fmt " LR(1) state"
        |> (fun formatter -> match nremaining with 1L -> formatter | _ -> formatter |> Fmt.fmt "s")
        |> Fmt.fmt "\n"
        |> Io.with_log io
      in
      (* Create a map that reindexes the remaining states. *)
      let remaining_state_index_map =
        Ordset.foldi ~init:(Ordmap.empty (module State.Index))
          ~f:(fun i remaining_state_index_map state_index ->
            Ordmap.insert_hlt ~k:state_index ~v:i remaining_state_index_map
          ) remaining_state_indexes in
      (* Create a map that reindexes the remaining states *and* maps the removed states to the
       * states they were remerged with. *)
      let reindexing_state_index_map = Ordmap.fold ~init:remaining_state_index_map
          ~f:(fun state_index_map (index0, index1) ->
            assert State.Index.(index0 > index1);
            Ordmap.insert_hlt ~k:index0 ~v:(Ordmap.get_hlt index1 remaining_state_index_map)
              state_index_map
          ) remergeable_index_map in
      (* Remerge isocores. *)
      let remerged_isocores = Ordmap.fold ~init:isocores
          ~f:(fun remerged_isocores (index0, index1) ->
            assert State.Index.(index0 > index1);
            let remerged_isocores =
              Isocores.remerge symbols remergeable_index_map index0 index1 remerged_isocores in
            remerged_isocores
          ) remergeable_index_map in
      (* Create a new set of reindexed isocores. *)
      let reindexed_isocores = Isocores.reindex reindexing_state_index_map remerged_isocores in
      (* Remerge states. *)
      let remerged_states = Ordmap.fold ~init:states
          ~f:(fun remerged_states (index0, index1) ->
            assert State.Index.(index0 > index1);
            let state0 = Array.get index0 states in
            let state1 = Array.get index1 states in
            let state1' = State.remerge symbols remergeable_index_map state0 state1 in
            let remerged_states = Array.set index1 state1' remerged_states in
            remerged_states
          ) remergeable_index_map in
      (* Create a new set of reindexed states. *)
      let reindexed_states =
        Array.fold ~init:(Ordset.empty (module State)) ~f:(fun reindexed_states state ->
          let state_index = State.index state in
          match Ordmap.mem state_index remaining_state_index_map with
          | false -> reindexed_states
          | true -> begin
              let reindexed_state = State.reindex reindexing_state_index_map state in
              Ordset.insert reindexed_state reindexed_states
            end
        ) remerged_states
        |> Ordset.to_array in
      io, reindexed_isocores, reindexed_states
    end

and init_inner algorithm ~resolve io precs symbols prods callbacks =
  let io, isocores, gotonub_of_statenub_goto =
    isocores_init algorithm ~resolve io precs symbols prods callbacks in
  let io, states = states_init io ~resolve symbols prods isocores ~gotonub_of_statenub_goto in
  io, isocores, states

and init algorithm ~resolve io hmh =
  let io =
    io.log
    |> Fmt.fmt "hocc: Generating "
    |> Fmt.fmt (match algorithm with
      | Conf.Lr1 -> "LR(1)"
      | Conf.Ielr1 -> "IELR(1)"
      | Conf.Pgm1 -> "PGM(1)"
      | Conf.Lalr1 -> "LALR(1)"
    )
    |> Fmt.fmt " specification\n"
    |> Io.with_log io
  in
  let io, precs, symbols, prods, callbacks = hmh_extract io hmh in
  let io, isocores, states = init_inner algorithm ~resolve io precs symbols prods callbacks in
  let io, isocores, states = gc_states io isocores states in
  let io, _isocores, states = remerge_states io symbols isocores states in
  let io = log_unused io precs symbols prods states in
  io, {algorithm; precs; symbols; prods; callbacks; states}

let conflicts {states; _} =
  match Array.reduce ~f:Uns.(+)
    (Array.map ~f:(fun state -> State.conflicts ~filter_pseudo_end:false state) states) with
  | None -> 0L
  | Some conflicts -> conflicts
