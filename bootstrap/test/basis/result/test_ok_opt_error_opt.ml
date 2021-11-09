open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "ok_opt %a -> %a\n"
      (xpp String.xpp String.xpp) result
      (Option.xpp String.xpp) (ok_opt result);
    printf "error_opt %a -> %a\n"
      (xpp String.xpp String.xpp) result
      (Option.xpp String.xpp) (error_opt result);
  );
  printf "@]"

let _ = test ()
