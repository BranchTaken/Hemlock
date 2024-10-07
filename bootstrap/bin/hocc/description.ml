open Basis
open! Basis.Rudiments

type description =
  | DescriptionTxt
  | DescriptionHtml

let generate_description conf io description Spec.{algorithm; precs; symbols; prods; states; _} =
  let sink _ formatter = formatter in
  let passthrough s formatter = formatter |> Fmt.fmt s in
  let txt = match description with
    | DescriptionTxt -> passthrough
    | DescriptionHtml -> sink
  in
  let html = match description with
    | DescriptionTxt -> sink
    | DescriptionHtml -> passthrough
  in
  let pp_symbol_index symbol_index formatter = begin
    let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
    let pretty_name = match symbol.alias with
      | None -> symbol.name
      | Some alias ->
        String.Fmt.empty
        |> txt "\"" |> html "“"
        |> Fmt.fmt alias
        |> txt "\"" |> html "”"
        |> Fmt.to_string
    in
    formatter |> html "<a href=\"#symbol-" |> html symbol.name |> html "\">"
    |> Fmt.fmt pretty_name |> html "</a>"
  end in
  let pp_symbol_set symbol_set formatter = begin
    formatter
    |> Fmt.fmt "{"
    |> (fun formatter ->
      Ordset.foldi ~init:formatter ~f:(fun i formatter symbol_index ->
        formatter
        |> (fun formatter -> match i with 0L -> formatter | _ -> formatter |> Fmt.fmt ", ")
        |> pp_symbol_index symbol_index
      ) symbol_set
    )
    |> Fmt.fmt "}"
  end in
  let pp_prec prec_ind formatter = begin
    let ref_name = (Precs.prec_of_prec_index prec_ind precs).name in
    formatter
    |> Fmt.fmt "prec " |> html "<a href=\"#prec-" |> html ref_name |> html "\">"
    |> Fmt.fmt ref_name
    |> html "</a>"
  end in
  let pp_prod ?(do_pp_prec=true) Prod.{lhs_index; rhs_indexes; prec; _} formatter = begin
    let lhs_name = Symbol.name (Symbols.symbol_of_symbol_index lhs_index symbols) in
    formatter
    |> html "<a href=\"#symbol-" |> html lhs_name |> html "\">"
    |> Fmt.fmt lhs_name
    |> html "</a>" |> Fmt.fmt " ::="
    |> (fun formatter ->
      match Array.length rhs_indexes with
      | 0L -> formatter |> Fmt.fmt " epsilon"
      | _ -> begin
          Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
            let rhs_name = Symbol.name (Symbols.symbol_of_symbol_index rhs_index symbols) in
            formatter
            |> Fmt.fmt " "
            |> html "<a href=\"#symbol-" |> html rhs_name |> html "\">"
            |> pp_symbol_index rhs_index
            |> html "</a>"
          ) rhs_indexes
        end
    )
    |> (fun formatter ->
      match do_pp_prec, prec with
      | false, _
      | _, None -> formatter
      | true, Some {index=prec_ind; _} -> formatter |> Fmt.fmt " " |> pp_prec prec_ind
    )
  end in
  let pp_lr0item lr0item formatter = begin
    let Lr0Item.{prod; dot} = lr0item in
    let Prod.{lhs_index; rhs_indexes; _} = prod in
    formatter
    |> Fmt.fmt (Symbol.name (Symbols.symbol_of_symbol_index lhs_index symbols))
    |> Fmt.fmt " ::="
    |> (fun formatter ->
      Array.foldi ~init:formatter ~f:(fun i formatter rhs_index ->
        formatter
        |> Fmt.fmt (match i = dot with
          | false -> ""
          | true -> " ·"
        )
        |> Fmt.fmt " "
        |> pp_symbol_index rhs_index
      ) rhs_indexes
      |> Fmt.fmt (
        match Array.length rhs_indexes = dot with
        | false -> ""
        | true -> " ·"
      )
    )
  end in
  let pp_lr1item ?(do_pp_prec=true) lr1item formatter = begin
    let Lr1Item.{lr0item; _} = lr1item in
    let Lr0Item.{prod; _} = lr0item in
    let Prod.{prec; _} = prod in
    formatter
    |> Fmt.fmt "["
    |> pp_lr0item lr0item
    |> Fmt.fmt ", {"
    |> (fun formatter ->
      Array.foldi ~init:formatter ~f:(fun i formatter symbol_index ->
        formatter
        |> Fmt.fmt (match i with
          | 0L -> ""
          | _ -> ", "
        )
        |> pp_symbol_index symbol_index
      ) (Ordset.to_array Lr1Item.(lr1item.follow))
    )
    |> Fmt.fmt "}]"
    |> (fun formatter ->
      match do_pp_prec, prec with
      | false, _
      | _, None -> formatter
      | true, Some {index=prec_index; _} -> formatter |> Fmt.fmt " " |> pp_prec prec_index
    )
  end in
  let pp_state_index state_index formatter = begin
    let state_index_string = String.Fmt.empty |> State.Index.pp state_index |> Fmt.to_string in
    formatter
    |> html "<a href=\"#state-" |> html state_index_string |> html "\">"
    |> Fmt.fmt state_index_string
    |> html "</a>"
  end in
  let pp_action symbol_index action formatter = begin
    let pp_symbol_prec symbol_index formatter = begin
      let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
      match symbol.prec with
      | None -> formatter
      | Some Prec.{index; _} -> formatter |> Fmt.fmt " " |> pp_prec index
    end in
    let pp_reduce_prec Prod.{lhs_index; prec; _} formatter = begin
      match prec with
      | Some _ -> formatter
      | None -> formatter |> pp_symbol_prec lhs_index
    end in
    let open State.Action in
    match action with
    | ShiftPrefix state_index ->
      formatter
      |> Fmt.fmt "ShiftPrefix " |> pp_state_index state_index
      |> pp_symbol_prec symbol_index
    | ShiftAccept state_index ->
      formatter
      |> Fmt.fmt "ShiftAccept " |> pp_state_index state_index
      |> pp_symbol_prec symbol_index
    | Reduce prod_index -> begin
        let prod = Prods.prod_of_prod_index prod_index prods in
        formatter |> Fmt.fmt "Reduce " |> pp_prod prod
        |> pp_reduce_prec prod
      end
  end in
  let pp_contrib contrib formatter = begin
    assert (not (Contrib.mem_shift contrib));
    match Contrib.length contrib with
    | 1L -> begin
        let prod_index = Contrib.reduces contrib |> Ordset.choose_hlt in
        let prod = Prods.prod_of_prod_index prod_index prods in
        formatter
        |> Fmt.fmt "Reduce "
        |> pp_prod ~do_pp_prec:false prod
       end
    | _ as ncontribs -> begin
        formatter
        |> html "<ul type=none>" |> Fmt.fmt "\n"
        |> (fun formatter ->
          Ordset.foldi ~init:formatter ~f:(fun i formatter prod_index ->
            let prod = Prods.prod_of_prod_index prod_index prods in
            formatter
            |> Fmt.fmt "                    " |> html "<li>"
            |> Fmt.fmt "Reduce "
            |> pp_prod ~do_pp_prec:false prod
            |> html "</li>" |> Fmt.fmt (match (succ i) < ncontribs with true -> "\n" | false -> "")
          ) (Contrib.reduces contrib)
        )
        |> html "</ul>"
      end
  end in
  let io =
    io.log
    |> Fmt.fmt "hocc: Generating "
    |> txt "text" |> html "html"
    |> Fmt.fmt " report\n"
    |> Io.with_log io
  in
  let nprecs = Precs.length precs in
  let states_algorithm = match Conf.algorithm conf with
    | Lr1 -> "LR(1)"
    | Ielr1 -> "IELR(1)"
    | Pgm1 -> "PGM(1)"
    | Lalr1 -> "LALR(1)"
  in
  (match description with
    | DescriptionTxt -> io.txt
    | DescriptionHtml -> io.html
  )
  |> html "<html>\n"
  |> html "<body>\n"
  |> html "<h1>" |> Fmt.fmt (Path.Segment.to_string_hlt (Conf.module_ conf))
  |> Fmt.fmt " grammar" |> html "</h1>" |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> html "<h2>Sections</h2>\n"
  |> html "    <ul type=none>\n"
  |> (fun formatter -> match nprecs with
    | 0L -> formatter
    | _ -> formatter |> html "    <li><a href=\"#precedences\">Precedences</a></li>\n"
  )
  |> html "    <li><a href=\"#tokens\">Tokens</a></li>\n"
  |> html "    <li><a href=\"#nonterms\">Non-terminals</a></li>\n"
  |> html "    <li><a href=\"#states\">" |> html states_algorithm |> html " States</a></li>\n"
  |> html "    </ul>\n"
  |> html "<hr>\n"
  |> (fun formatter -> match nprecs with
    | 0L -> formatter
    | _ ->
      formatter |> html "<h2 id=\"precedences\">" |> Fmt.fmt "Precedences"
      |> (fun formatter -> match (Conf.resolve conf) with
        | true -> formatter
        | false -> formatter |> Fmt.fmt " (conflict resolution disabled)"
      )
      |> html "</h2>"
      |> Fmt.fmt"\n"
  )
  |> html "    <ul>\n"
  |> (fun formatter ->
    Precs.fold ~init:formatter ~f:(fun formatter Prec.{name; assoc; doms; _} ->
      formatter
      |> Fmt.fmt "    " |> html "<li>"
      |> Fmt.fmt (match assoc with
        | None -> "neutral"
        | Some Left -> "left"
        | Some Right -> "right"
      )
      |> Fmt.fmt " " |> html "<a id=\"prec-" |> html name |> html "\">"
      |> Fmt.fmt name
      |> html "</a>"
      |> (fun formatter ->
        match Ordset.is_empty doms with
        | true -> formatter
        | false -> begin
            let _, formatter = Ordset.fold ~init:(true, formatter)
              ~f:(fun (first, formatter) prec_ind ->
                let ref_name = (Precs.prec_of_prec_index prec_ind precs).name in
                let formatter =
                  formatter
                  |> Fmt.fmt (match first with
                    | true -> " < "
                    | false -> ", "
                  )
                  |> html "<a href=\"#prec-" |> html ref_name |> html "\">"
                  |> Fmt.fmt ref_name
                  |> html "</a>"
                in
                (false, formatter)
              ) doms
            in
            formatter
          end
      )
      |> html "</li>" |> Fmt.fmt "\n"
    ) precs
  )
  |> html "    </ul>\n"
  |> html "<h2 id=\"tokens\">" |> Fmt.fmt "Tokens" |> html "</h2>" |> Fmt.fmt "\n"
  |> html "    <ul>\n"
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter
      ~f:(fun formatter (Symbol.{name; alias; stype; prec; first; follow; _} as symbol) ->
        match Symbol.is_token symbol with
        | false -> formatter
        | true -> begin
            formatter
            |> Fmt.fmt "    " |> html "<li>" |> Fmt.fmt "token "
            |> html "<a id=\"symbol-" |> html name |> html "\">"
            |> Fmt.fmt name
            |> html "</a>"
            |> (fun formatter ->
              match alias with
              | None -> formatter
              | Some alias -> formatter |> Fmt.fmt " " |> String.pp alias
            )
            |> (fun formatter ->
              match SymbolType.is_explicit stype with
              | false -> formatter
              | true ->
                formatter |> Fmt.fmt " of " |> Fmt.fmt (SymbolType.to_string stype)
            )
            |> (fun formatter ->
              match prec with
              | None -> formatter
              | Some {index=prec_index; _} -> formatter |> Fmt.fmt " " |> pp_prec prec_index
            )
            |> Fmt.fmt "\n"
            |> html "        <ul type=none>\n"
            |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "First: "
            |> pp_symbol_set first
            |> html "</li>" |> Fmt.fmt "\n"
            |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Follow: "
            |> pp_symbol_set follow
            |> html "</li>" |> Fmt.fmt "\n"
            |> html "        </ul>\n"
            |> html "    </li>\n"
          end
      ) symbols
  )
  |> html "    </ul>\n"
  |> html "<h2 id=\"nonterms\">" |> Fmt.fmt "Non-terminals" |> html "</h2>" |> Fmt.fmt "\n"
  |> html "    <ul>\n"
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter
      ~f:(fun formatter (Symbol.{name; start; stype; prods; first; follow; _} as symbol) ->
        match Symbol.is_nonterm symbol with
        | false -> formatter
        | true -> begin
            formatter
            |> Fmt.fmt "    " |> html "<li>"
            |> Fmt.fmt (match start with
              | true -> "start "
              | false -> "nonterm "
            )
            |> html "<a id=\"symbol-" |> html name |> html "\">"
            |> Fmt.fmt name
            |> html "</a>"
            |> (fun formatter ->
              match SymbolType.is_explicit stype with
              | false -> formatter
              | true ->
                formatter |> Fmt.fmt " of " |> Fmt.fmt (SymbolType.to_string stype)
            )
            |> Fmt.fmt "\n"
            |> html "            <ul type=none>\n"
            |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "First: "
            |> pp_symbol_set first
            |> html "</li>" |> Fmt.fmt "\n"
            |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Follow: "
            |> pp_symbol_set follow
            |> html "</li>" |> Fmt.fmt "\n"
            |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Productions\n"
            |> html "            <ul type=none>\n"
            |> (fun formatter ->
              Ordset.fold ~init:formatter
                ~f:(fun formatter prod ->
                  formatter
                  |> Fmt.fmt "            " |> html "<li>"
                  |> pp_prod prod
                  |> html "</li>" |> Fmt.fmt "\n"
                ) prods
              |> html "            </ul>\n"
              |> html "        </li>\n"
            )
            |> html "        </ul>\n"
            |> html "    </li>\n"
          end
      ) symbols
  )
  |> html "    </ul>\n"
  |> html "<h2 id=\"states\">" |> Fmt.fmt states_algorithm |> Fmt.fmt " States" |> html "</h2>"
  |> Fmt.fmt "\n"
  |> html "    <ul>\n"
  |> (fun formatter ->
    Array.fold ~init:formatter
      ~f:(fun formatter (State.{statenub; actions; gotos; _} as state) ->
        let state_index_string =
          String.Fmt.empty |> StateNub.Index.pp (StateNub.index statenub)
          |> Fmt.to_string in
        formatter
        |> Fmt.fmt "    " |> html "<li>" |> Fmt.fmt "State "
        |> html "<a id=\"state-" |> html state_index_string |> html "\">"
        |> Fmt.fmt state_index_string
        |> (fun formatter ->
          match algorithm with
          | Lr1
          | Ielr1
          | Pgm1 -> begin
              formatter
              |> Fmt.fmt " ["
              |> Uns.pp (StateNub.isocores_sn statenub)
              |> Fmt.fmt "."
              |> Uns.pp (StateNub.isocore_set_sn statenub)
              |> Fmt.fmt "]"
            end
          | Lalr1 -> formatter
        )
        |> html "</a>" |> Fmt.fmt "\n"
        |> html "        <ul type=none>\n"
        |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Kernel\n"
        |> html "            <ul type=none>\n"
        |> (fun formatter ->
          Lr1Itemset.fold ~init:formatter ~f:(fun formatter lr1itemset ->
            formatter
            |> Fmt.fmt "            " |> html "<li>"
            |> pp_lr1item lr1itemset
            |> html "</li>" |> Fmt.fmt "\n"
          ) statenub.lr1itemsetclosure.kernel
        )
        |> html "            </ul>\n"
        |> html "        </li>\n"
        |> (fun formatter ->
          match Lr1Itemset.is_empty statenub.lr1itemsetclosure.added with
          | true -> formatter
          | false -> begin
              formatter
              |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Added\n"
              |> html "            <ul type=none>\n"
              |> (fun formatter ->
                Lr1Itemset.fold ~init:formatter ~f:(fun formatter lr1itemset ->
                  formatter |> Fmt.fmt "            " |> html "<li>"
                  |> pp_lr1item lr1itemset
                  |> html "</li>" |> Fmt.fmt "\n"
                ) statenub.lr1itemsetclosure.added
              )
              |> html "            </ul>\n"
              |> html "        </li>\n"
            end
        )
        |> (fun formatter ->
          let has_pseudo_end_conflict = State.has_pseudo_end_conflict state in
          formatter
          |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Actions\n"
          |> html "            <ul type=none>\n"
          |> (fun formatter ->
            Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, action_set) ->
              formatter
              |> (fun formatter ->
                match has_pseudo_end_conflict && symbol_index = Symbol.pseudo_end.index with
                | false -> formatter |> Fmt.fmt "            " |> html "<li>"
                | true -> formatter |> txt "CONFLICT    " |> html "            <li>CONFLICT "
              )
              |> pp_symbol_index symbol_index |> Fmt.fmt " :"
              |> (fun formatter ->
                match Ordset.length action_set with
                | 1L -> begin
                    formatter
                    |> Fmt.fmt " "
                    |> pp_action symbol_index (Ordset.choose_hlt action_set)
                    |> html "</li>" |> Fmt.fmt "\n"
                  end
                | _ -> begin
                    formatter
                    |> html " CONFLICT" |> Fmt.fmt "\n"
                    |> html "                <ul type=none>\n"
                    |> (fun formatter ->
                      Ordset.fold ~init:formatter ~f:(fun formatter action ->
                        formatter
                        |> txt "CONFLICT        " |> html "                <li>"
                        |> pp_action symbol_index action
                        |> html "</li>" |> Fmt.fmt "\n"
                      ) action_set
                    )
                    |> html "                </ul>\n"
                  end
              )
            ) actions
          )
          |> html "            </ul>\n"
          |> html "        </li>\n"
        )
        |> (fun formatter ->
          match Ordmap.is_empty gotos with
          | true -> formatter
          | false -> begin
              formatter
              |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Gotos\n"
              |> html "            <ul type=none>\n"
              |> (fun formatter ->
                Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, state_index) ->
                  formatter
                  |> Fmt.fmt "            " |> html "<li>"
                  |> pp_symbol_index symbol_index |> Fmt.fmt " : " |> State.Index.pp state_index
                  |> html "</li>" |> Fmt.fmt "\n"
                ) gotos
              )
              |> html "            </ul>\n"
              |> html "        </li>\n"
            end
        )
        |> (fun formatter ->
          let kernel_attribs = StateNub.filtered_kernel_attribs statenub in
          match KernelAttribs.length kernel_attribs with
          | 0L -> formatter
          | _ -> begin
              let kernel_attribs = StateNub.filtered_kernel_attribs statenub in
              formatter
              |> Fmt.fmt "        " |> html "<li>" |> Fmt.fmt "Conflict contributions\n"
              |> html "            <ul type=none>\n"
              |> (fun formatter ->
                KernelAttribs.fold ~init:formatter ~f:(fun formatter (kernel_item, attribs) ->
                  formatter
                  |> Fmt.fmt "            " |> pp_lr1item ~do_pp_prec:false kernel_item
                  |> Fmt.fmt "\n"
                  |> html "                <ul type=none>\n"
                  |> (fun formatter ->
                    Attribs.fold ~init:formatter
                      ~f:(fun formatter Attrib.{conflict_state_index; contrib; _} ->
                        formatter
                        |> Fmt.fmt "                " |> html "<li>"
                        |> pp_state_index conflict_state_index
                        |> Fmt.fmt " : "
                        |> pp_contrib contrib
                        |> html "</li>" |> Fmt.fmt "\n"
                      ) attribs
                  )
                  |> html "                </ul>\n"
                ) kernel_attribs
              )
              |> html "            </ul>\n"
              |> html "        </li>\n"
            end
        )
        |> html "        </ul>\n"
        |> html "    </li>\n"
      ) states
  )
  |> html "    </ul>\n"
  |> html "</body>\n"
  |> html "</html>\n"
  |> (match description with
    | DescriptionTxt -> Io.with_txt io
    | DescriptionHtml -> Io.with_html io
  )

let generate_txt conf io spec =
  generate_description conf io DescriptionTxt spec

let generate_html conf io spec =
  generate_description conf io DescriptionHtml spec
