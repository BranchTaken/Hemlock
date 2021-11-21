open! Basis.Rudiments
open! Basis
open Option

let test () =
  let pp_ab (a, b) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> Uns.pp a
    |> Fmt.fmt ", "
    |> String.pp b
    |> Fmt.fmt ")"
  end in
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      File.Fmt.stdout
      |> Fmt.fmt "both ("
      |> (pp Uns.pp) o0
      |> Fmt.fmt ") ("
      |> (pp String.pp) o1
      |> Fmt.fmt ") -> "
      |> (pp pp_ab) (both o0 o1)
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
