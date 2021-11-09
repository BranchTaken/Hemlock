open RudimentsFunctions

module T = struct
  type t = unit

  let hash_fold _t state =
    (* The hash of unit is constant, but it still needs to be folded. *)
    state
    |> Uns.hash_fold 0L

  let cmp _ _ =
    Cmp.Eq

  let xpp xppf _t =
    Format.fprintf xppf "()"

  let of_string s =
    match s with
    | "unit"
    | "()" -> ()
    | _ -> not_reached ()

  let to_string _ =
    "()"

  let fmt t formatter =
    formatter |> Fmt.fmt (to_string t)
end
include T
include Identifiable.Make(T)
