include RangeH.Uns
type range = t

let pp_l pp_a l formatter =
  match l with
  | RangeIntf.Overflow -> formatter |> Fmt.fmt "Overflow"
  | RangeIntf.Length a -> formatter |> Fmt.fmt "Length " |> pp_a a
