open Basis
open! Basis.Rudiments

module T = struct
  type t =
    | Left
    | Right

  let hash_fold t state =
    state |> Uns.hash_fold (match t with
      | Left -> 0L
      | Right -> 1L
    )

  let cmp t0 t1 =
    let open Cmp in
    match t0, t1 with
    | Left, Right -> Lt
    | Left, Left
    | Right, Right -> Eq
    | Right, Left -> Gt

  let pp t formatter =
    formatter |> Fmt.fmt (match t with
      | Left -> "Left"
      | Right -> "Right"
    )
end
include T
include Identifiable.Make(T)
