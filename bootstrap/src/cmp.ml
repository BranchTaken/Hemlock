type t =
  | Lt
  | Eq
  | Gt

let pp ppf t =
  Format.fprintf ppf (match t with
    | Lt -> "Lt"
    | Eq -> "Eq"
    | Gt -> "Gt"
  )
