include Radix0

let pp t formatter =
  formatter |> Fmt.fmt (
    match t with
    | Bin -> "Bin"
    | Oct -> "Oct"
    | Dec -> "Dec"
    | Hex -> "Hex"
  )

let to_uns = function
  | Bin -> 2L
  | Oct -> 8L
  | Dec -> 10L
  | Hex -> 16L
