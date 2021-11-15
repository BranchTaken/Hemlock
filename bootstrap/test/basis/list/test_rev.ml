open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
  ] in
  iter lists ~f:(fun l ->
    File.Fmt.stdout
    |> Fmt.fmt "rev "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (rev l)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
