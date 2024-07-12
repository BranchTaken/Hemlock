open Basis
open Basis.Rudiments

module T = struct
  type t = {
    prod: Prod.t;
    dot: uns;
  }

  let hash_fold {prod; dot} state =
    state
    |> Prod.hash_fold prod
    |> Uns.hash_fold dot

  let cmp {prod=p0; dot=d0} {prod=p1; dot=d1} =
    let open Cmp in
    match Prod.cmp p0 p1 with
    | Lt -> Lt
    | Eq -> Uns.cmp d0 d1
    | Gt -> Gt

  let pp {prod; dot} formatter =
    formatter
    |> Fmt.fmt "{prod=" |> Prod.pp prod
    |> Fmt.fmt "; dot=" |> Uns.pp dot
    |> Fmt.fmt "}"

  let pp_hr symbols {prod; dot} formatter =
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
        |> Symbol.pp_hr (Symbols.symbol_of_symbol_index rhs_index symbols)
      ) rhs_indexes
      |> Fmt.fmt (
        match Array.length rhs_indexes = dot with
        | false -> ""
        | true -> " ·"
      )
    )
end
include T
include Identifiable.Make(T)

let init ~prod ~dot =
  {prod; dot}
