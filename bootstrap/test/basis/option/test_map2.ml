open! Basis.Rudiments
open! Basis
open Option

let test () =
  let pp_ab (a, b) formatter = begin
    formatter
    |> Uns.pp a
    |> Fmt.fmt ", "
    |> String.pp b
  end in
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      File.Fmt.stdout
      |> Fmt.fmt "map2 ("
      |> (pp Uns.pp) o0
      |> Fmt.fmt ") ("
      |> (pp String.pp) o1
      |> Fmt.fmt ") ~f:(fun a b -> (a, b))) -> "
      |> (pp pp_ab) (map2 o0 o1 ~f:(fun a b -> (a, b)))
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
