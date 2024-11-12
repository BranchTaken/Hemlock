open Basis
open! Basis.Rudiments

let generate_hocc io Spec.{precs; symbols; _} =
  let io = io.log |> Fmt.fmt "hocc: Generating hocc report\n" |> Io.with_log io in
  io.hocc
  |> Fmt.fmt "hocc\n"
  |> (fun formatter ->
    Precs.fold_prec_sets ~init:formatter ~f:(fun formatter prec_set ->
      formatter |> PrecSet.src_fmt prec_set
    ) precs
  )
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter ~f:(fun formatter symbol ->
      match Symbol.is_token symbol && not (Symbol.is_synthetic symbol) with
      | false -> formatter
      | true -> formatter |> Symbols.src_fmt symbol symbols
    ) symbols
  )
  |> (fun formatter ->
    Symbols.symbols_fold ~init:formatter ~f:(fun formatter symbol ->
      match Symbol.is_nonterm symbol && not (Symbol.is_synthetic symbol) with
      | false -> formatter
      | true -> formatter |> Symbols.src_fmt symbol symbols
    ) symbols
  )
  |> Io.with_hocc io
