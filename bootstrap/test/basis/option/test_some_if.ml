open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [false; true] ~f:(fun b ->
    let a = 42L in
    File.Fmt.stdout
    |> Fmt.fmt "some_if "
    |> Bool.pp b
    |> Fmt.fmt " "
    |> Uns.pp a
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (some_if b a)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
