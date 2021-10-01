open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 1L)) ~f:(fun i ->
    let t0 = of_sint i in
    RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 1L)) ~f:(fun j ->
      let t1 = of_sint j in
      printf "min max %.1f %.1f -> %.1f %.1f\n" t0 t1 (min t0 t1) (max t0 t1);
    );
  );
  printf "@]"

let _ = test ()
