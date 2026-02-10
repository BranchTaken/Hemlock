open Basis
open! Basis.Rudiments

let generate_txt conf io Spec.{algorithm; precs; symbols; prods; states; _} =
  let pp_symbol_index symbol_index formatter = begin
    let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
    let pretty_name = match symbol.alias with
      | None -> symbol.name
      | Some alias ->
        String.Fmt.empty
        |> Fmt.fmt "\""
        |> Fmt.fmt alias
        |> Fmt.fmt "\""
        |> Fmt.to_string
    in
    formatter
    |> Fmt.fmt pretty_name
  end in
  let pp_symbol_set symbol_set formatter = begin
    formatter
    |> Fmt.fmt "{"
    |> (fun formatter ->
      Bitset.foldi ~init:formatter ~f:(fun i formatter symbol_index ->
        formatter
        |> (fun formatter -> match i with 0L -> formatter | _ -> formatter |> Fmt.fmt ", ")
        |> pp_symbol_index symbol_index
      ) symbol_set
    )
    |> Fmt.fmt "}"
  end in
  let pp_prec name formatter = begin
    formatter
    |> Fmt.fmt "prec "
    |> Fmt.fmt name
  end in
  let pp_prod ?(do_pp_prec=true) Prod.{lhs_index; rhs_indexes; prec; _} formatter = begin
    let lhs_name = Symbol.name (Symbols.symbol_of_symbol_index lhs_index symbols) in
    formatter
    |> Fmt.fmt lhs_name
    |> Fmt.fmt " ::="
    |> (fun formatter ->
      match Array.length rhs_indexes with
      | 0L -> formatter |> Fmt.fmt " epsilon"
      | _ -> begin
          Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
            formatter
            |> Fmt.fmt " "
            |> pp_symbol_index rhs_index
          ) rhs_indexes
        end
    )
    |> (fun formatter ->
      match do_pp_prec, prec with
      | false, _
      | _, None -> formatter
      | true, Some prec -> formatter |> Fmt.fmt " " |> pp_prec (Prec.name prec)
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
      ) (Bitset.to_array Lr1Item.(lr1item.follow))
    )
    |> Fmt.fmt "}]"
    |> (fun formatter ->
      match do_pp_prec, prec with
      | false, _
      | _, None -> formatter
      | true, Some prec -> formatter |> Fmt.fmt " " |> pp_prec (Prec.name prec)
    )
  end in
  let pp_state_index state_index formatter = begin
    let state_index_string = String.Fmt.empty |> State.Index.pp state_index |> Fmt.to_string in
    formatter
    |> Fmt.fmt state_index_string
  end in
  let pp_action symbol_index action formatter = begin
    let pp_symbol_prec symbol_index formatter = begin
      let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
      match symbol.prec with
      | None -> formatter
      | Some prec -> formatter |> Fmt.fmt " " |> pp_prec (Prec.name prec)
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
        |> Fmt.fmt "\n"
        |> (fun formatter ->
          Ordset.foldi ~init:formatter ~f:(fun i formatter prod_index ->
            let prod = Prods.prod_of_prod_index prod_index prods in
            formatter
            |> Fmt.fmt "                    "
            |> Fmt.fmt "Reduce "
            |> pp_prod ~do_pp_prec:false prod
            |> Fmt.fmt (match (succ i) < ncontribs with true -> "\n" | false -> "")
          ) (Contrib.reduces contrib)
        )
      end
  end in
  let io =
    io.log
    |> Fmt.fmt "hocc: Generating text report\n"
    |> Io.with_log io
  in
  let nprecs = Precs.length precs in
  let states_algorithm = match Conf.algorithm conf with
    | Lr1 -> "LR(1)"
    | Ielr1 -> "IELR(1)"
    | Pgm1 -> "PGM(1)"
    | Lalr1 -> "LALR(1)"
  in
  io.txt
  |> Fmt.fmt (Path.Segment.to_string_hlt (Conf.module_ conf))
  |> Fmt.fmt " grammar" |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> (fun formatter -> match nprecs with
    | 0L -> formatter
    | _ -> formatter
  )
  |> (fun formatter -> match nprecs with
    | 0L -> formatter
    | _ ->
      formatter |> Fmt.fmt "Precedences"
      |> (fun formatter -> match (Conf.resolve conf) with
        | true -> formatter
        | false -> formatter |> Fmt.fmt " (conflict resolution disabled)"
      )
      |> Fmt.fmt"\n"
  )
  |> (fun formatter ->
    Precs.fold_prec_sets ~init:formatter ~f:(fun formatter PrecSet.{names; assoc; doms; _} ->
      formatter
      |> Fmt.fmt "    "
      |> Fmt.fmt (match assoc with
        | None -> "neutral"
        | Some Left -> "left"
        | Some Right -> "right"
        | Some Nonassoc -> "nonassoc"
      )
      |> (fun formatter ->
        Array.fold ~init:formatter ~f:(fun formatter name ->
          formatter
          |> Fmt.fmt " "
          |> Fmt.fmt name
        ) names
      )
      |> (fun formatter ->
        match Bitset.is_empty doms with
        | true -> formatter
        | false -> begin
            let _, formatter = Bitset.fold ~init:(true, formatter)
              ~f:(fun (first, formatter) prec_ind ->
                Array.fold ~init:(first, formatter) ~f:(fun (first, formatter) name ->
                  let formatter =
                    formatter
                    |> Fmt.fmt (match first with
                      | true -> " < "
                      | false -> ", "
                    )
                    |> Fmt.fmt name
                  in
                  (false, formatter)
                ) (Precs.prec_set_of_prec_index prec_ind precs).names
              ) doms
            in
            formatter
          end
      )
      |> Fmt.fmt "\n"
    ) precs
  )
  |> Fmt.fmt "Tokens" |> Fmt.fmt "\n"
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter
      ~f:(fun formatter (Symbol.{name; alias; stype; prec; first; follow; _} as symbol) ->
        match Symbol.is_token symbol with
        | false -> formatter
        | true -> begin
            formatter
            |> Fmt.fmt "    " |> Fmt.fmt "token "
            |> Fmt.fmt name
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
              | Some prec -> formatter |> Fmt.fmt " " |> pp_prec (Prec.name prec)
            )
            |> Fmt.fmt "\n"
            |> Fmt.fmt "        First: "
            |> pp_symbol_set first
            |> Fmt.fmt "\n"
            |> Fmt.fmt "        Follow: "
            |> pp_symbol_set follow
            |> Fmt.fmt "\n"
          end
      ) symbols
  )
  |> Fmt.fmt "Non-terminals" |> Fmt.fmt "\n"
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter
      ~f:(fun formatter (Symbol.{name; start; stype; prods; first; follow; _} as symbol) ->
        match Symbol.is_nonterm symbol with
        | false -> formatter
        | true -> begin
            formatter
            |> Fmt.fmt "    "
            |> Fmt.fmt (match start with
              | true -> "start "
              | false -> "nonterm "
            )
            |> Fmt.fmt name
            |> (fun formatter ->
              match SymbolType.is_explicit stype with
              | false -> formatter
              | true ->
                formatter |> Fmt.fmt " of " |> Fmt.fmt (SymbolType.to_string stype)
            )
            |> Fmt.fmt "\n"
            |> Fmt.fmt "        First: "
            |> pp_symbol_set first
            |> Fmt.fmt "\n"
            |> Fmt.fmt "        Follow: "
            |> pp_symbol_set follow
            |> Fmt.fmt "\n"
            |> Fmt.fmt "        Productions\n"
            |> (fun formatter ->
              Ordset.fold ~init:formatter
                ~f:(fun formatter prod ->
                  formatter
                  |> Fmt.fmt "            "
                  |> pp_prod prod
                  |> Fmt.fmt "\n"
                ) prods
            )
          end
      ) symbols
  )
  |> Fmt.fmt states_algorithm |> Fmt.fmt " States"
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    Array.fold ~init:formatter
      ~f:(fun formatter (State.{statenub; actions; gotos; _} as state) ->
        let state_index_string =
          String.Fmt.empty |> StateNub.Index.pp (StateNub.index statenub)
          |> Fmt.to_string in
        formatter
        |> Fmt.fmt "    State "
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
        |> Fmt.fmt "\n"
        |> Fmt.fmt "        Kernel\n"
        |> (fun formatter ->
          Lr1Itemset.fold ~init:formatter ~f:(fun formatter lr1itemset ->
            formatter
            |> Fmt.fmt "            "
            |> pp_lr1item lr1itemset
            |> Fmt.fmt "\n"
          ) statenub.lr1itemsetclosure.kernel
        )
        |> (fun formatter ->
          match Lr1Itemset.is_empty statenub.lr1itemsetclosure.added with
          | true -> formatter
          | false -> begin
              formatter
              |> Fmt.fmt "        Added\n"
              |> (fun formatter ->
                Lr1Itemset.fold ~init:formatter ~f:(fun formatter lr1itemset ->
                  formatter |> Fmt.fmt "            "
                  |> pp_lr1item lr1itemset
                  |> Fmt.fmt "\n"
                ) statenub.lr1itemsetclosure.added
              )
            end
        )
        |> (fun formatter ->
          let has_pseudo_end_conflict = State.has_pseudo_end_conflict state in
          formatter
          |> Fmt.fmt "        Actions\n"
          |> (fun formatter ->
            Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, action_set) ->
              formatter
              |> (fun formatter ->
                match has_pseudo_end_conflict && symbol_index = Symbol.pseudo_end.index with
                | false -> formatter |> Fmt.fmt "            "
                | true -> formatter |> Fmt.fmt "CONFLICT    "
              )
              |> pp_symbol_index symbol_index |> Fmt.fmt " :"
              |> (fun formatter ->
                match Ordset.length action_set with
                | 1L -> begin
                    formatter
                    |> Fmt.fmt " "
                    |> pp_action symbol_index (Ordset.choose_hlt action_set)
                    |> Fmt.fmt "\n"
                  end
                | _ -> begin
                    formatter
                    |> Fmt.fmt "\n"
                    |> (fun formatter ->
                      Ordset.fold ~init:formatter ~f:(fun formatter action ->
                        formatter
                        |> Fmt.fmt "CONFLICT        "
                        |> pp_action symbol_index action
                        |> Fmt.fmt "\n"
                      ) action_set
                    )
                  end
              )
            ) actions
          )
        )
        |> (fun formatter ->
          match Ordmap.is_empty gotos with
          | true -> formatter
          | false -> begin
              formatter
              |> Fmt.fmt "        Gotos\n"
              |> (fun formatter ->
                Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, state_index) ->
                  formatter
                  |> Fmt.fmt "            "
                  |> pp_symbol_index symbol_index |> Fmt.fmt " : " |> State.Index.pp state_index
                  |> Fmt.fmt "\n"
                ) gotos
              )
            end
        )
        |> (fun formatter ->
          let kernel_attribs = StateNub.filtered_kernel_attribs statenub in
          match KernelAttribs.length kernel_attribs with
          | 0L -> formatter
          | _ -> begin
              let kernel_attribs = StateNub.filtered_kernel_attribs statenub in
              formatter
              |> Fmt.fmt "        Conflict contributions\n"
              |> (fun formatter ->
                KernelAttribs.fold ~init:formatter ~f:(fun formatter (kernel_item, attribs) ->
                  formatter
                  |> Fmt.fmt "            " |> pp_lr1item ~do_pp_prec:false kernel_item
                  |> Fmt.fmt "\n"
                  |> (fun formatter ->
                    Attribs.fold ~init:formatter
                      ~f:(fun formatter Attrib.{conflict_state_index; contrib; _} ->
                        formatter
                        |> Fmt.fmt "                "
                        |> pp_state_index conflict_state_index
                        |> Fmt.fmt " : "
                        |> pp_contrib contrib
                        |> Fmt.fmt "\n"
                      ) attribs
                  )
                ) kernel_attribs
              )
            end
        )
      ) states
  )
  |> Io.with_txt io
