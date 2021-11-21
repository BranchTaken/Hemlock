open! Basis.Rudiments
open! Basis
open Option

let test () =
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some 13L; None] ~f:(fun o1 ->
      File.Fmt.stdout
      |> Fmt.fmt "first_some ("
      |> (pp Uns.pp) o0
      |> Fmt.fmt ") ("
      |> (pp Uns.pp) o1
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) (first_some o0 o1)
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
