open RudimentsFunctions

type t =
  | Neg
  | Zero
  | Pos

let pp ppf t =
  Format.fprintf ppf (match t with
    | Neg -> "Neg"
    | Zero -> "Zero"
    | Pos -> "Pos"
  )
let of_sint x =
  match x with
  | -1 -> Neg
  | 0 -> Zero
  | 1 -> Pos
  | _ -> not_reached ()

let to_sint t =
  match t with
  | Neg -> ~-1
  | Zero -> 0
  | Pos -> 1

let to_real t =
  match t with
  | Neg -> -1.
  | Zero -> 0.
  | Pos -> 1.

let flip t =
  match t with
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg

let ( * ) t0 t1 =
  match t0, t1 with
  | Neg, Pos
  | Pos, Neg -> Neg
  | Zero, _
  | _, Zero -> Zero
  | Neg, Neg
  | Pos, Pos -> Pos
