open Basis
open! Basis.Rudiments

module T = struct
  type t =
    | Synthetic
    | Implicit
    | Explicit of {
        module_: string;
        type_: string;
      }

  let hash_fold t state =
    match t with
    | Synthetic -> state |> Uns.hash_fold 0L
    | Implicit -> state |> Uns.hash_fold 1L
    | Explicit {module_; type_} -> begin
        state
        |> Uns.hash_fold 2L
        |> String.hash_fold module_
        |> String.hash_fold type_
      end

  let cmp t0 t1 =
    let open Cmp in
    match t0, t1 with
    | Synthetic, Synthetic -> Eq
    | Synthetic, (Implicit|Explicit _) -> Lt
    | Implicit, Synthetic -> Gt
    | Implicit, Implicit -> Eq
    | Implicit, Explicit _ -> Lt
    | Explicit _, (Synthetic|Implicit) -> Gt
    | Explicit {module_=m0; type_=t0}, Explicit {module_=m1; type_=t1} -> begin
        match String.cmp m0 m1 with
        | Lt -> Lt
        | Eq -> String.cmp t0 t1
        | Gt -> Gt
      end

  let pp t formatter =
    match t with
    | Synthetic -> formatter |> Fmt.fmt "Synthetic"
    | Implicit -> formatter |> Fmt.fmt "Implicit"
    | Explicit {module_; type_} ->
      formatter
      |> Fmt.fmt "Explicit {module_=" |> String.pp module_
      |> Fmt.fmt "; type_=" |> String.pp type_
      |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let synthetic = Synthetic

let implicit = Implicit

let init ~module_ ~type_ =
  Explicit {module_; type_}
