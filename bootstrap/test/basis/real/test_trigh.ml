open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  for i = -2 to 2 do
    let i = Sint.extend_of_int i in
    let t = of_sint i in
    printf ("sinh cosh tanh %.1f -> (%.5f %.5f) (%.5f %.5f) (%.5f %.5f)\n")
      t
      (sinh t) (((ex t) - (ex ~-t)) / 2.)
      (cosh t) (((ex t) + (ex ~-t)) / 2.)
      (tanh t) ((sinh t) / (cosh t));
  done;
  printf "@]"

let _ = test ()
