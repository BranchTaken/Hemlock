open! Basis.Rudiments
open! Basis
open Result
open Format

let test () =
  let result_pairs = [
    (Ok "ok0", Ok "ok1");
    (Error "error0", Ok "ok1");
    (Ok "ok0", Error "error1");
    (Error "error0", Error "error1")
  ] in
  printf "@[<h>";
  List.iter result_pairs ~f:(fun (a, b) ->
    let f a b = asprintf "%s + %s" a b in
    printf "merge (%a) (%a) -> (%a)\n"
      (xpp String.xpp String.xpp) a
      (xpp String.xpp String.xpp) b
      (xpp String.xpp String.xpp) (merge a b ~ok:f ~error:f)
  );
  printf "@]"

let _ = test ()
