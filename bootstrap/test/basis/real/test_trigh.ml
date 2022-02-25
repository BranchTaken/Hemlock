open! Basis.Rudiments
open! Basis
open Real

let test () =
  Range.Sint.(iter Sint.(kv (-2L) =:= kv 2L)) ~f:(fun i ->
    let t = of_sint i in
    File.Fmt.stdout
    |> Fmt.fmt "sinh cosh tanh "
    |> fmt ~pmode:Fmt.Fixed ~precision:2L ~notation:Fmt.RadixPoint t
    |> Fmt.fmt " -> ("
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (sinh t)
    |> Fmt.fmt " "
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (((ex t) - (ex ~-t)) / 2.)
    |> Fmt.fmt ") ("
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (cosh t)
    |> Fmt.fmt " "
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (((ex t) + (ex ~-t)) / 2.)
    |> Fmt.fmt ") ("
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (tanh t)
    |> Fmt.fmt " "
    |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint ((sinh t) / (cosh t))
    |> Fmt.fmt ")\n"
    |> ignore
  )

let _ = test ()
