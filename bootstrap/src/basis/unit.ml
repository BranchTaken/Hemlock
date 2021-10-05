open RudimentsFunctions

module T = struct
  type t = unit

  let hash_fold _t state =
    (* The hash of unit is constant, but it still needs to be folded. *)
    state
    |> Uns.hash_fold 0L

  let cmp _ _ =
    Cmp.Eq

  let pp ppf _t =
    Format.fprintf ppf "()"

  let of_string s =
    match s with
    | "unit"
    | "()" -> ()
    | _ -> not_reached ()

  let to_string _ =
    "()"
end
include T
include Identifiable.Make(T)
