type t =
| Lt
| Eq
| Gt

let of_string s =
  match s with
  | "Lt" -> Lt
  | "Eq" -> Eq
  | "Gt" -> Gt
  | _ -> assert false

let to_string t =
  match t with
  | Lt -> "Lt"
  | Eq -> "Eq"
  | Gt -> "Gt"
