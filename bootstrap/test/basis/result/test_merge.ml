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
      (pp String.pp String.pp) a
      (pp String.pp String.pp) b
      (pp String.pp String.pp) (merge a b ~ok:f ~error:f)
  );
  printf "@]"

let _ = test ()
