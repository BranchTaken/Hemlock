open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [false; true] ~f:(fun b ->
    List.iter [Some 42L; None] ~f:(fun o ->
      File.Fmt.stdout
      |> Fmt.fmt "filter "
      |> (pp Uns.pp) o
      |> Fmt.fmt " ~f:(fun _ -> "
      |> Bool.pp b
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) (filter o ~f:(fun _ -> b))
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
