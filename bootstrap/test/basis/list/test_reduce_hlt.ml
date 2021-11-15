open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
    [0L; 1L; 2L; 3L; 4L]
  ] in
  iter lists ~f:(fun l ->
    File.Fmt.stdout
    |> Fmt.fmt "reduce "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> Option.fmt Uns.pp (reduce l ~f:(fun a b -> a + b))
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
