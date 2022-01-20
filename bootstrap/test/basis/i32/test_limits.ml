open! Basis.Rudiments
open! Basis
open I32

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "bit_length="
  |> Uns.pp (bit_pop (bit_not zero))
  |> Fmt.fmt "\nmin_value="
  |> pp min_value
  |> Fmt.fmt " "
  |> fmt ~alt:true ~zpad:true ~width:8L ~radix:Radix.Hex ~pretty:true min_value
  |> Fmt.fmt "\nmax_value="
  |> pp max_value
  |> Fmt.fmt " "
  |> fmt ~alt:true ~zpad:true ~width:8L ~radix:Radix.Hex ~pretty:true max_value
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
