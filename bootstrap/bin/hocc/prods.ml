open! Basis
open! Basis.Rudiments

type t = Prod.t array

module Builder = struct
  type outer = t
  type t = (Prod.Index.t, Prod.t, Prod.Index.cmper_witness) Ordmap.t

  let empty = Ordmap.empty (module Prod.Index)

  let insert ~lhs_index ~rhs_indexes ~prec ~stmt ~callback t =
    let index = Ordmap.length t in
    let prod = Prod.init ~index ~lhs_index ~rhs_indexes ~prec ~stmt ~callback in
    prod, Ordmap.insert_hlt ~k:index ~v:prod t

  let build t =
    Array.init (0L =:< Ordmap.length t) ~f:(fun index -> Ordmap.get_hlt index t)
end

let length = Array.length

let prod_of_prod_index = Array.get

let fold = Array.fold

let src_fmt precs symbols Prod.{lhs_index; rhs_indexes; prec; _} formatter =
  let lhs_symbol = Symbols.symbol_of_symbol_index lhs_index symbols in
  formatter
  |> Fmt.fmt "    "
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
    | Some prec -> formatter |> Fmt.fmt " " |> Precs.pp_prec_hr prec precs
  )
  |> Fmt.fmt "\n"
