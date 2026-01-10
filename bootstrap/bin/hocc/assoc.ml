open Basis
open! Basis.Rudiments

module T = struct
  type t =
    | Left
    | Right
    | Nonassoc

  let to_uns = function
    | Left -> 0L
    | Right -> 1L
    | Nonassoc -> 2L

  let hash_fold t state =
    state |> Uns.hash_fold (to_uns t)

  let cmp t0 t1 =
    Uns.cmp (to_uns t0) (to_uns t1)

  let pp t formatter =
    formatter |> Fmt.fmt (match t with
      | Left -> "Left"
      | Right -> "Right"
      | Nonassoc -> "Nonassoc"
    )
end
include T
include Identifiable.Make(T)
