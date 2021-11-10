open! Basis.Rudiments
open! Basis
open Unit

let test () =
  let t0 = () in
  let t1 = () in
  File.Fmt.stdout
  |> Fmt.fmt "cmp " |> Fmt.fmt (to_string t0) |> Fmt.fmt " " |> Fmt.fmt (to_string t1)
  |> Fmt.fmt " -> " |> Cmp.pp (cmp t0 t1) |> Fmt.fmt "\n"
  |> Fmt.fmt (to_string t0) |> Fmt.fmt " = " |> Fmt.fmt (to_string t1) |> Fmt.fmt " -> "
  |> Bool.pp (t0 = t1)
  |> Fmt.fmt "\n"
  |> Fmt.fmt (to_string t0) |> Fmt.fmt " <> " |> Fmt.fmt (to_string t1) |> Fmt.fmt " -> "
  |> Bool.pp (t0 <> t1) |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
