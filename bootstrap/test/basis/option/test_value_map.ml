open! Basis.Rudiments
open! Basis
open Option

let test () =
  let default = 13L in
  let replacement = 43L in
  List.iter [Some 42L; None] ~f:(fun o ->
    File.Fmt.stdout
    |> Fmt.fmt "value_map "
    |> (pp Uns.pp) o
    |> Fmt.fmt " ~default:"
    |> Uns.pp default
    |> Fmt.fmt " ~f:(fun _ -> "
    |> Uns.pp replacement
    |> Fmt.fmt ") -> "
    |> Uns.pp (value_map o ~default ~f:(fun _ -> replacement))
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
