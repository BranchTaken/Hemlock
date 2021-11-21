open! Basis.Rudiments
open! Basis
open Option

let test () =
  let replacement = 77L in
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some 43L; None] ~f:(fun o1 ->
      File.Fmt.stdout
      |> Fmt.fmt "merge ("
      |> (pp Uns.pp) o0
      |> Fmt.fmt ") ("
      |> (pp Uns.pp) o1
      |> Fmt.fmt ") ~f:(fun _ _ -> "
      |> Uns.pp replacement
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) (merge o0 o1 ~f:(fun _ _ -> replacement))
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
