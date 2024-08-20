open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    synthetic: bool; (** Synthetic symbol if true. *)
    symbol_type: string list; (** Implicit if empty. Optional qualifiers prefix type, which is in
                                  last element. *)
  }

  let hash_fold {synthetic; symbol_type} state =
    state
    |> Bool.hash_fold synthetic
    |> List.hash_fold String.hash_fold symbol_type

  let pp {synthetic; symbol_type} formatter =
    formatter
    |> Fmt.fmt "{synthetic=" |> Bool.pp synthetic
    |> Fmt.fmt "; symbol_type=" |> List.pp String.pp symbol_type
    |> Fmt.fmt "}"

  let cmp {synthetic=s0; symbol_type=st0} {synthetic=s1; symbol_type=st1} =
    let open Cmp in
    match Bool.cmp s0 s1 with
    | Lt -> Lt
    | Eq -> List.cmp String.cmp st0 st1
    | Gt -> Gt
end
include T
include Identifiable.Make(T)

let is_synthetic {synthetic; _} =
  synthetic

let is_explicit {symbol_type; _} =
  not (List.is_empty symbol_type)

let to_string ({symbol_type; _} as t) =
  assert (is_explicit t);
  String.join ~sep:"." symbol_type

let synthetic_implicit =
  {synthetic=true; symbol_type=[]}

let implicit =
  {synthetic=false; symbol_type=[]}

let synthetic_explicit type_ =
  {synthetic=true; symbol_type=[type_]}

let explicit type_ =
  {synthetic=false; symbol_type=[type_]}

let qualify qualifier ({symbol_type; _} as t) =
  assert (is_explicit t);
  {t with symbol_type=qualifier :: symbol_type}

let synthetic_wrapper {synthetic; symbol_type} =
  assert (not synthetic);
  {synthetic=true; symbol_type}
