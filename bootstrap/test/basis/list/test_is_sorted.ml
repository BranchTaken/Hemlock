open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];

    [0L; 0L];
    [0L; 1L; 1L];
    [0L; 1L; 2L; 2L];

    [1L; 0L];
    [0L; 2L; 1L]
  ] in
  iter lists ~f:(fun l ->
    File.Fmt.stdout
    |> Fmt.fmt "is_sorted               "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> Bool.pp (is_sorted l ~cmp:Uns.cmp)
    |> Fmt.fmt "\nis_sorted ~strict:false "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> Bool.pp (is_sorted ~strict:false l ~cmp:Uns.cmp)
    |> Fmt.fmt "\nis_sorted ~strict:true  "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> Bool.pp (is_sorted ~strict:true l ~cmp:Uns.cmp)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
