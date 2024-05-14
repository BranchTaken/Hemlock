open Basis
open! Basis.Rudiments

module T = struct
  type t =
    | No
    | Maybe
    | Yes

  let to_uns = function
    | No -> 0L
    | Maybe -> 1L
    | Yes -> 2L

  let hash_fold t state =
    state
    |> Uns.hash_fold (to_uns t)

  let cmp t0 t1 =
    Uns.cmp (to_uns t0) (to_uns t1)

  let of_string = function
    | "No" -> No
    | "Maybe" -> Maybe
    | "Yes" -> Yes
    | _ -> not_reached ()

  let to_string = function
    | No -> "No"
    | Maybe -> "Maybe"
    | Yes -> "Yes"

  let pp t formatter =
    formatter
    |> Fmt.fmt (to_string t)
end
include T
include Identifiable.Make(T)
