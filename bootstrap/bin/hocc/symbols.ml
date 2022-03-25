open Basis
open! Basis.Rudiments

type info = {
  index: Symbol.Index.t;
  name: string;
  alias: string option;
  qtype: QualifiedType.t;
}

type t = {
  infos: (string, info, String.cmper_witness) Map.t;
  names: (string, Symbol.Index.t, String.cmper_witness) Map.t;
  aliases: (string, Symbol.Index.t, String.cmper_witness) Map.t;
  symbols: (Symbol.Index.t, Symbol.t, Symbol.Index.cmper_witness) Ordmap.t;
  tokens: (Symbol.Index.t, Symbol.t, Symbol.Index.cmper_witness) Ordmap.t;
  nonterms: (Symbol.Index.t, Symbol.t, Symbol.Index.cmper_witness) Ordmap.t;
}

let empty =
  let infos, names, aliases, symbols = List.fold
      ~init:(Map.empty (module String), Map.empty (module String), Map.empty (module String),
        Ordmap.empty (module Symbol.Index))
      ~f:(fun (infos, names, aliases, symbols) (Symbol.{index; name; qtype; alias; _} as token) ->
        let info = {index; name; alias; qtype} in
        let infos' = Map.insert_hlt ~k:name ~v:info infos in
        let names' = Map.insert_hlt ~k:name ~v:index names in
        let aliases' = Map.insert_hlt ~k:(Option.value_hlt alias) ~v:index aliases in
        let symbols' = Ordmap.insert ~k:index ~v:token symbols in
        (infos', names', aliases', symbols')
      ) [Symbol.epsilon; Symbol.pseudo_end]
  in
  {infos; names; aliases; symbols; tokens=symbols; nonterms=Ordmap.empty (module Symbol.Index)}

let info_of_name name {infos; _} =
  Map.get name infos

let info_of_name_hlt name t =
  Option.value_hlt (info_of_name name t)

let info_of_alias alias ({aliases; tokens; _} as t) =
  match Map.get alias aliases with
  | None -> None
  | Some symbol_index -> info_of_name Symbol.((Ordmap.get_hlt symbol_index tokens).name) t

let insert_token ~name ~qtype ~prec ~stmt ~alias
    ({infos; names; aliases; symbols; tokens; _} as t) =
  let index = Map.length infos in
  let info = {index; name; alias; qtype} in
  let token = Symbol.init_token ~index ~name ~qtype ~prec ~stmt ~alias in
  let infos' = Map.insert_hlt ~k:name ~v:info infos in
  let names' = Map.insert_hlt ~k:name ~v:index names in
  let aliases' = match alias with
    | None -> aliases
    | Some alias -> Map.insert_hlt ~k:alias ~v:index aliases
  in
  let symbols' = Ordmap.insert ~k:index ~v:token symbols in
  let tokens' = Ordmap.insert ~k:index ~v:token tokens in
  {t with infos=infos'; names=names'; aliases=aliases'; symbols=symbols'; tokens=tokens'}

let insert_nonterm_info ~name ~qtype ({infos; _} as t) =
  let index = Map.length infos in
  let info = {index; name; alias=None; qtype} in
  let infos' = Map.insert_hlt ~k:name ~v:info infos in
  {t with infos=infos'}

let insert_nonterm ~name ~prec ~stmt ~start ~prods ({names; symbols; nonterms; _} as t) =
  let {index; qtype; _} = info_of_name_hlt name t in
  let nonterm = Symbol.init_nonterm ~index ~name ~qtype ~prec ~stmt ~start ~prods in
  let names' = Map.insert_hlt ~k:name ~v:index names in
  let symbols' = Ordmap.insert ~k:index ~v:nonterm symbols in
  let nonterms' = Ordmap.insert ~k:index ~v:nonterm nonterms in
  {t with names=names'; symbols=symbols'; nonterms=nonterms'}

let update_symbol (Symbol.{index; _} as symbol) ({symbols; tokens; nonterms; _} as t) =
  let symbols' = Ordmap.update_hlt ~k:index ~v:symbol symbols in
  let tokens', nonterms' = match Symbol.is_token symbol with
    | true -> Ordmap.update_hlt ~k:index ~v:symbol tokens, nonterms
    | false -> tokens, Ordmap.update_hlt ~k:index ~v:symbol nonterms
  in
  {t with symbols=symbols'; tokens=tokens'; nonterms=nonterms'}

let symbol_index_of_name name {names; _} =
  Map.get name names

let symbol_of_name name ({symbols; _} as t) =
  match symbol_index_of_name name t with
  | None -> None
  | Some symbol_index -> Some (Ordmap.get_hlt symbol_index symbols)

let symbol_index_of_alias alias {aliases; _} =
  Map.get alias aliases

let symbol_of_alias alias ({symbols; _} as t) =
  match symbol_index_of_alias alias t with
  | None -> None
  | Some symbol_index -> Some (Ordmap.get_hlt symbol_index symbols)

let symbol_of_symbol_index index {symbols; _} =
  Ordmap.get_hlt index symbols

let symbols_length {symbols; _} =
  Ordmap.length symbols

let tokens_length {tokens; _} =
  Ordmap.length tokens

let nonterms_length {nonterms; _} =
  Ordmap.length nonterms

let fold_impl ~init ~f symbols =
  Ordmap.fold ~init ~f:(fun accum (_, symbol) -> f accum symbol) symbols

let symbols_fold ~init ~f {symbols; _} =
  fold_impl ~init ~f symbols

let tokens_fold ~init ~f {tokens; _} =
  fold_impl ~init ~f tokens

let nonterms_fold ~init ~f {nonterms; _} =
  fold_impl ~init ~f nonterms

let src_fmt (Symbol.{name; prec; alias; start; prods; _} as symbol) t formatter =
  match Symbol.is_token symbol with
  | true -> begin
      formatter
      |> Fmt.fmt "    token "
      |> Fmt.fmt name
      |> (fun formatter ->
        match alias with
        | None -> formatter
        | Some alias -> formatter |> Fmt.fmt " " |> String.pp alias
      )
      |> (fun formatter ->
        match prec with
        | None -> formatter
        | Some {name; _} -> formatter |> Fmt.fmt " prec " |> Fmt.fmt name
      )
      |> Fmt.fmt "\n"
    end
  | false -> begin
      formatter
      |> Fmt.fmt (match start with
        | true -> "    start "
        | false -> "    nonterm "
      )
      |> Fmt.fmt name
      |> (fun formatter ->
        match prec with
        | None -> formatter
        | Some {name; _} ->
          formatter |> Fmt.fmt " prec " |> Fmt.fmt name
      )
      |> Fmt.fmt " ::="
      |> Fmt.fmt (match Ordset.length prods with
        | 1L -> ""
        | _ -> "\n"
      )
      |> (fun formatter ->
        let symbol_prec = prec in
        Ordset.fold ~init:formatter ~f:(fun formatter Prod.{rhs_indexes; prec; _} ->
          formatter
          |> Fmt.fmt (match Ordset.length prods with
            | 1L -> ""
            | _ -> "      |"
          )
          |> (fun formatter ->
            match Array.length rhs_indexes with
            | 0L -> formatter |> Fmt.fmt " epsilon"
            | _ -> begin
                Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
                  let rhs_symbol = symbol_of_symbol_index rhs_index t in
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
            match symbol_prec, prec with
            | None, None
            | Some _, Some _ (* Re-normalize; prec was propagated from symbol. *)
              -> formatter
            | None, Some {name; _} -> formatter |> Fmt.fmt " prec " |> Fmt.fmt name
            | Some _, None -> not_reached ()
          )
          |> Fmt.fmt "\n"
        ) prods
      )
    end

let pp_symbol_hr (Symbol.{name; alias; _} as symbol) formatter =
  match Symbol.is_token symbol with
  | true -> formatter |> Fmt.fmt (match alias with None -> name | Some alias -> alias)
  | false -> formatter |> Fmt.fmt name

let pp_prod_hr Prod.{lhs_index; rhs_indexes; _} t formatter =
  formatter
  |> pp_symbol_hr (symbol_of_symbol_index lhs_index t)
  |> Fmt.fmt " ::="
  |> (fun formatter ->
    match Array.length rhs_indexes with
    | 0L -> formatter |> Fmt.fmt " epsilon"
    | _ -> begin
        Array.fold ~init:formatter ~f:(fun formatter rhs_index ->
          let rhs_symbol = symbol_of_symbol_index rhs_index t in
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
