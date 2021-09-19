open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  let results_lists = [
    [ (Ok "ok0"); (Ok "ok1"); (Ok "ok2")];
    [ (Ok "ok0"); (Error "error0"); (Ok "ok1"); (Error "error1"); (Ok "ok2")];
  ] in
  printf "@[<h>";
  List.iter results_lists ~f:(fun results ->
    printf "all %a -> %a\n"
      (List.pp (pp String.pp String.pp)) results
      (pp (List.pp String.pp) (List.pp String.pp)) (all results)
  );
  printf "@]"

let _ = test ()
