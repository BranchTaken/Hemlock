open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  printf "@[<h>";
  List.iter [true; false] ~f:(fun b ->
    printf "ok_if %b -> %a\n" b (pp Unit.pp String.pp) (ok_if b ~error:"oops");
    printf "error_if %b -> %a\n"
      b (pp String.pp Unit.pp) (error_if b ~ok:"whew")
  );
  printf "@]"

let _ = test ()
