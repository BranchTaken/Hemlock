open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "ok_opt %a -> %a\n"
      (pp String.pp String.pp) result
      (Option.pp String.pp) (ok_opt result);
    printf "error_opt %a -> %a\n"
      (pp String.pp String.pp) result
      (Option.pp String.pp) (error_opt result);
  );
  printf "@]"

let _ = test ()
