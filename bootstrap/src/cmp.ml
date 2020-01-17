type t =
| Lt
| Eq
| Gt
[@@deriving sexp]

let pp ppf t =
  Format.fprintf ppf "%s" (Sexplib.Sexp.to_string (sexp_of_t t))
