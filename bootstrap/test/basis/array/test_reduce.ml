open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_reduce arr ~f = begin
    printf "reduce %a" (xpp Uns.xpp) arr;
    match reduce arr ~f with
    | None -> printf " -> None\n"
    | Some x -> printf " -> %a\n" Uns.xpp x
  end in
  let f a b = (a + b) in
  printf "@[<h>";
  test_reduce [||] ~f;
  test_reduce [|0L; 1L; 2L; 3L; 4L|] ~f;
  printf "@]"

let _ = test ()
