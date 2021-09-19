open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  let f msg = asprintf "%s'" msg in
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "map_ok %a -> %a\n"
      (pp String.pp String.pp) result
      (pp String.pp String.pp) (map_ok result ~f);
    printf "map_error %a -> %a\n"
      (pp String.pp String.pp) result
      (pp String.pp String.pp) (map_error result ~f);
  );
  printf "@]"

let _ = test ()
