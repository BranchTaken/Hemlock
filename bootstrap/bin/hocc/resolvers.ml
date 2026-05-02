open Basis
open! Basis.Rudiments

type t = {
  assoc: (PrecSet.Index.t, PrecSet.Index.cmper_witness) Ordset.t;
  token_prec: Bitset.t;
  prod_prec: (Prod.Index.t, Prod.Index.cmper_witness) Ordset.t;
}

let pp {assoc; token_prec; prod_prec} formatter =
  formatter
  |> Fmt.fmt "{assoc=" |> Ordset.pp assoc
  |> Fmt.fmt "; token_prec=" |> Bitset.pp token_prec
  |> Fmt.fmt "; prod_prec=" |> Ordset.pp prod_prec
  |> Fmt.fmt "}"

let empty = {
  assoc=Ordset.empty (module PrecSet.Index);
  token_prec=Bitset.empty;
  prod_prec=Ordset.empty (module Prod.Index);
}

let union {assoc=a0; token_prec=tp0; prod_prec=pp0} {assoc=a1; token_prec=tp1; prod_prec=pp1} =
  {assoc=Ordset.union a0 a1; token_prec=Bitset.union tp0 tp1; prod_prec=Ordset.union pp0 pp1}

let use_assoc prec_set_index ({assoc; _} as t) =
  {t with assoc=Ordset.insert prec_set_index assoc}

let use_token_prec token_index ({token_prec; _} as t) =
  {t with token_prec=Bitset.insert token_index token_prec}

let use_prod_prec prod_index ({prod_prec; _} as t) =
  {t with prod_prec=Ordset.insert prod_index prod_prec}

let is_assoc_useful prec_set_index {assoc; _} =
  Ordset.mem prec_set_index assoc

let is_token_prec_useful token_index {token_prec; _} =
  Bitset.mem token_index token_prec

let is_prod_prec_useful prod_index {prod_prec; _} =
  Ordset.mem prod_index prod_prec
