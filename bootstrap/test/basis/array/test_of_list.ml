open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test list = begin
    File.Fmt.stdout
    |> Fmt.fmt "of_list[_rev] "
    |> (List.pp Uns.pp) list
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (of_list list)
    |> Fmt.fmt " / "
    |> (pp Uns.pp) (of_list_rev list)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
  ] in
  List.iter lists ~f:test

let _ = test ()
