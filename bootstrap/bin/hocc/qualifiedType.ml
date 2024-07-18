open Basis
open! Basis.Rudiments

module T = struct
  type explicit = {
    module_: string;
    type_: string;
  }
  type t = {
    synthetic: bool;
    explicit_opt: explicit option;
  }

  let hash_fold {synthetic; explicit_opt} state =
    state
    |> Bool.hash_fold synthetic
    |> (fun state ->
      match explicit_opt with
      | None -> state |> Uns.hash_fold 0L
      | Some {module_; type_} -> begin
          state
          |> Uns.hash_fold 1L
          |> String.hash_fold module_
          |> String.hash_fold type_
        end
    )

  let cmp {synthetic=s0; explicit_opt=e0} {synthetic=s1; explicit_opt=e1} =
    let open Cmp in
    match Bool.cmp s0 s1 with
    | Lt -> Lt
    | Eq -> begin
        match e0, e1 with
        | None, None -> Eq
        | None, Some _ -> Lt
        | Some {module_=m0; type_=t0}, Some {module_=m1; type_=t1} -> begin
            match String.cmp m0 m1 with
            | Lt -> Lt
            | Eq -> String.cmp t0 t1
            | Gt -> Gt
          end
        | Some _, None -> Gt
      end
    | Gt -> Gt

  let pp {synthetic; explicit_opt} formatter =
    let pp_explicit {module_; type_} formatter = begin
      formatter
      |> Fmt.fmt "{module_=" |> String.pp module_
      |> Fmt.fmt "; type_=" |> String.pp type_
      |> Fmt.fmt "}"
    end in
    formatter
    |> Fmt.fmt "{synthetic=" |> Bool.pp synthetic
    |> Fmt.fmt "; explicit_opt=" |> Option.pp pp_explicit explicit_opt
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let synthetic_implicit =
  {synthetic=true; explicit_opt=None}

let implicit =
  {synthetic=false; explicit_opt=None}

let synthetic_explicit ~module_ ~type_ =
  {synthetic=true; explicit_opt=Some {module_; type_}}

let explicit ~module_ ~type_ =
  {synthetic=false; explicit_opt=Some {module_; type_}}

let synthetic_wrapper {synthetic; explicit_opt} =
  assert (not synthetic);
  {synthetic=true; explicit_opt}
