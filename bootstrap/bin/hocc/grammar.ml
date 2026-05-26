open Basis
open! Basis.Rudiments

let generate_hocc io Spec.{precs; symbols; _} =
  let io = io.log |> Fmt.fmt "hocc: Generating Hocc report\n" |> Io.with_log io in
  io.hocc
  |> Fmt.fmt "hocc\n"
  |> (fun formatter ->
    Precs.fold_prec_sets ~init:formatter ~f:(fun formatter prec_set ->
      formatter |> PrecSet.hocc_fmt prec_set
    ) precs
  )
  |> (fun formatter ->
    Symbols.tokens_fold ~init:formatter ~f:(fun formatter symbol ->
      match Symbol.is_synthetic symbol with
      | true -> formatter
      | false -> formatter |> Symbols.hocc_fmt precs symbol symbols
    ) symbols
  )
  |> (fun formatter ->
    Symbols.nonterms_fold ~init:formatter ~f:(fun formatter symbol ->
      match Symbol.is_synthetic symbol with
      | true -> formatter
      | false -> formatter |> Symbols.hocc_fmt precs symbol symbols
    ) symbols
  )
  |> Io.with_hocc io

let generate_yacc io Spec.{precs; symbols; _} =
  let io = io.log |> Fmt.fmt "hocc: Generating Yacc report\n" |> Io.with_log io in
  let nstarts = Symbols.nonterms_fold ~init:0L ~f:(fun nstarts ({start; _} as symbol) ->
    nstarts + (Bool.to_uns (start && not (Symbol.is_synthetic symbol)))
  ) symbols in
  io.yacc
  |> Fmt.fmt "%{\n"
  |> Fmt.fmt "%}\n\n"
  |> (fun formatter ->
    Precs.fold_prec_sets_right ~init:formatter ~f:(fun formatter {index; names; assoc; _} ->
      formatter
      |> Fmt.fmt (
        match assoc with
        | None -> "%precedence"
        | Some Left -> "%left"
        | Some Right -> "%right"
        | Some Nonassoc -> "%nonassoc"
      )
      |> Fmt.fmt " "
      |> Fmt.fmt (String.join ~sep:" " (Array.to_list names))
      |> (fun formatter ->
        Symbols.tokens_fold ~init:formatter ~f:(fun formatter {name; prec; _} ->
          match prec with
          | None -> formatter
          | Some {prec_set_index; _} -> begin
              let PrecSet.{index=i; _} = Precs.prec_set_of_prec_index prec_set_index precs in
              match i = index with
              | false -> formatter
              | true -> formatter |> Fmt.fmt " " |> Fmt.fmt name
            end
        ) symbols
      )
      |> Fmt.fmt "\n"
    ) precs
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    Symbols.tokens_fold ~init:formatter ~f:(fun formatter {name; stype; alias; _} ->
      match SymbolType.is_synthetic stype with
      | true -> formatter
      | false -> begin
          formatter
          |> Fmt.fmt "%token "
          |> Fmt.fmt name
          |> (fun formatter ->
            match alias with
            | None -> formatter
            | Some alias -> formatter |> Fmt.fmt " \"" |> Fmt.fmt alias |> Fmt.fmt "\""
          )
          |> Fmt.fmt "\n"
        end
    ) symbols
  )
  |> (fun formatter ->
    match nstarts with
    | 0L -> not_reached ()
    | 1L -> formatter
    | _ -> begin
        formatter
        |> Fmt.fmt "\n"
        |> (fun formatter ->
          Symbols.nonterms_fold ~init:formatter
            ~f:(fun formatter ({name; start; _} as symbol) ->
              match start && not (Symbol.is_synthetic symbol) with
              | false -> formatter
              | true -> begin
                  formatter
                  |> Fmt.fmt "%token HOCC_PSEUDO_START_" |> Fmt.fmt name |> Fmt.fmt "\n"
                end
            ) symbols
        )
      end
  )
  |> Fmt.fmt "\n%%\n\n"
  |> (fun formatter ->
    match nstarts with
    | 0L -> not_reached ()
    | 1L -> begin
        formatter
        |> (fun formatter ->
          Symbols.nonterms_fold ~init:formatter ~f:(fun formatter ({name; start; _} as symbol) ->
            match start && not (Symbol.is_synthetic symbol) with
            | false -> formatter
            | true -> formatter |> Fmt.fmt "%start " |> Fmt.fmt name |> Fmt.fmt ";\n"
          ) symbols
        )
      end
    | _ -> begin
        formatter
        |> Fmt.fmt "%start hocc-pseudo-start;\n"
        |> Fmt.fmt "\n"
        |> Fmt.fmt "hocc-pseudo-start:\n"
        |> (fun formatter ->
          let formatter, _first =
            Symbols.nonterms_fold ~init:(formatter, true)
              ~f:(fun (formatter, first) ({name; start; _} as symbol) ->
                let formatter, first =
                  match start && not (Symbol.is_synthetic symbol) with
                  | false -> formatter, first
                  | true -> begin
                      let formatter =
                        formatter
                        |> (fun formatter ->
                          match first with
                          | true -> formatter |> Fmt.fmt "  "
                          | false -> formatter |> Fmt.fmt "| "
                        )
                        |> Fmt.fmt "HOCC_PSEUDO_START_" |> Fmt.fmt name
                        |> Fmt.fmt " " |> Fmt.fmt name |> Fmt.fmt ";\n"
                      in
                      formatter, false
                    end
                in
                formatter, first
              ) symbols in
          formatter
        )
        |> Fmt.fmt ";\n"
      end
  )
  |> (fun formatter ->
    Symbols.nonterms_fold ~init:formatter
      ~f:(fun formatter ({name; prods; _} as symbol) ->
        match Symbol.is_synthetic symbol with
        | true -> formatter
        | false -> begin
            let has_epsilon_prod = Array.for_any ~f:(fun Prod.{rhs_indexes; _} ->
              Array.is_empty rhs_indexes
            ) prods in
            formatter
            |> Fmt.fmt "\n"
            |> Fmt.fmt name |> Fmt.fmt " :\n"
            |> (fun formatter ->
              match has_epsilon_prod with
              | false -> formatter
              | true -> begin
                  Array.fold ~init:formatter ~f:(fun formatter Prod.{rhs_indexes; prec; _} ->
                    formatter
                    |> (fun formatter ->
                      match Array.length rhs_indexes with
                      | 0L -> begin
                          formatter |> Fmt.fmt "  /* empty */"
                          |> (fun formatter ->
                            match prec with
                            | None -> formatter
                            | Some {name_index; prec_set_index} -> begin
                                let prec_set = Precs.prec_set_of_prec_index prec_set_index precs in
                                let name = PrecSet.name_of_name_index name_index prec_set in
                                formatter |> Fmt.fmt " %prec " |> Fmt.fmt name
                              end
                          )
                          |> Fmt.fmt "\n"
                        end
                      | _ -> formatter
                    )
                  ) prods
                end
            )
            |> (fun formatter ->
              let formatter, _first =
                Array.fold ~init:(formatter, not has_epsilon_prod)
                  ~f:(fun (formatter, first) Prod.{rhs_indexes; prec; _} ->
                    let formatter =
                      formatter
                      |> (fun formatter ->
                        match Array.length rhs_indexes with
                        | 0L -> formatter
                        | _ -> begin
                            formatter
                            |> (fun formatter ->
                              match first with
                              | true -> formatter |> Fmt.fmt " "
                              | false -> formatter |> Fmt.fmt "|"
                            )
                            |> (fun formatter ->
                              Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
                                let Symbol.{name; alias; _} =
                                  Symbols.symbol_of_symbol_index rhs_index symbols in
                                formatter
                                |> Fmt.fmt " "
                                |> (fun formatter ->
                                  match alias with
                                  | None -> formatter |> Fmt.fmt name
                                  | Some alias ->
                                    formatter |> Fmt.fmt "\"" |> Fmt.fmt alias |> Fmt.fmt "\""
                                )
                              ) rhs_indexes
                            )
                            |> (fun formatter ->
                              match prec with
                              | None -> formatter
                              | Some {name_index; prec_set_index} -> begin
                                  let prec_set =
                                    Precs.prec_set_of_prec_index prec_set_index precs in
                                  let name = PrecSet.name_of_name_index name_index prec_set in
                                  formatter |> Fmt.fmt " %prec " |> Fmt.fmt name
                                end
                            )
                            |> Fmt.fmt "\n"
                          end
                      )
                    in
                    formatter, false
                  ) prods
              in
              formatter
            )
            |> Fmt.fmt ";\n"
          end
      ) symbols
  )
  |> Fmt.fmt "\n%%\n"
  |> Io.with_yacc io
