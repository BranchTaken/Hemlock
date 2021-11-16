type ('a, 'b) t =
  | First of 'a
  | Second of 'b

let hash_fold hash_fold_a hash_fold_b t state =
  match t with
  | First a -> state |> Uns.hash_fold 0L |> hash_fold_a a
  | Second b -> state |> Uns.hash_fold 1L |> hash_fold_b b

let cmp cmp_a cmp_b t0 t1 =
  let open Cmp in
  match t0, t1 with
  | First a0, First a1 -> cmp_a a0 a1
  | First _, Second _ -> Lt
  | Second _, First _ -> Gt
  | Second b0, Second b1 -> cmp_b b0 b1

let xpp xpp_a xpp_b xppf = function
  | First a -> Format.fprintf xppf "@[<h>First %a@]" xpp_a a
  | Second b -> Format.fprintf xppf "@[<h>Second %a@]" xpp_b b

let fmt ?(alt=Fmt.alt_default) fmt_a fmt_b t formatter =
  match t with
  | First a ->
    formatter
    |> Fmt.fmt "First "
    |> Fmt.fmt (if alt then "(" else "")
    |> fmt_a a
    |> Fmt.fmt (if alt then ")" else "")
  | Second b ->
    formatter
    |> Fmt.fmt "Second "
    |> Fmt.fmt (if alt then "(" else "")
    |> fmt_b b
    |> Fmt.fmt (if alt then ")" else "")

let pp pp_a pp_b t formatter =
  fmt ~alt:true pp_a pp_b t formatter

let is_first = function
  | First _ -> true
  | Second _ -> false

let is_second = function
  | First _ -> false
  | Second _ -> true

let swap = function
  | First a -> Second a
  | Second b -> First b

let value = function
  | First a
  | Second a -> a

let value_map ~first ~second = function
  | First a -> first a
  | Second b -> second b

let map ~first ~second = function
  | First a -> First (first a)
  | Second b -> Second (second b)
