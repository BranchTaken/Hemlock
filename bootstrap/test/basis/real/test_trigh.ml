open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  RangeF.Sint.(iter Sint.(kv (-2L) =:= kv 2L)) ~f:(fun i ->
    let t = of_sint i in
    printf ("sinh cosh tanh %.1f -> (%.5f %.5f) (%.5f %.5f) (%.5f %.5f)\n")
      t
      (sinh t) (((ex t) - (ex ~-t)) / 2.)
      (cosh t) (((ex t) + (ex ~-t)) / 2.)
      (tanh t) ((sinh t) / (cosh t));
  );
  printf "@]"

let _ = test ()
