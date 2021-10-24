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

let fmt t ((module Formatter):(module Fmt.Formatter)) : (module Fmt.Formatter) =
  (module Formatter) |> Fmt.fmt (match t with
    | Lt -> "Lt"
    | Eq -> "Eq"
    | Gt -> "Gt"
  )

let is_ge = function
  | Lt -> false
  | Eq -> true
  | Gt -> true

let is_le = function
  | Lt -> true
  | Eq -> true
  | Gt -> false

let is_eq = function
  | Lt -> false
  | Eq -> true
  | Gt -> false

let is_gt = function
  | Lt -> false
  | Eq -> false
  | Gt -> true

let is_lt = function
  | Lt -> true
  | Eq -> false
  | Gt -> false

let is_ne = function
  | Lt -> true
  | Eq -> false
  | Gt -> true
