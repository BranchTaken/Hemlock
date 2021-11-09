open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  let f msg = asprintf "%s'" msg in
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "map_ok %a -> %a\n"
      (xpp String.xpp String.xpp) result
      (xpp String.xpp String.xpp) (map_ok result ~f);
    printf "map_error %a -> %a\n"
      (xpp String.xpp String.xpp) result
      (xpp String.xpp String.xpp) (map_error result ~f);
  );
  printf "@]"

let _ = test ()
