open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let norm_nan t = if (is_nan t) then nan else t in
  RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 2L)) ~f:(fun i ->
    let t0 = of_sint i in
    RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 2L)) ~f:(fun j ->
      let t1 = of_sint j in
      printf ("+ - * / %% ** copysign %.1f ~sign:%.1f -> " ^^
          "%.1f %.1f %.1f %.1f %.1f %.1f %.1f\n")
        t0 t1 (t0 + t1) (t0 - t1) (t0 * t1) (norm_nan (t0 / t1))
        (norm_nan (t0 % t1)) (t0 ** t1) (copysign ~sign:t1 t0);
    );
    printf "~- ~+ neg abs %.1f -> %.1f %.1f %.1f %.1f\n"
      t0 (~- t0) (~+ t0) (neg t0) (abs t0);
  );
  printf "@]"

let _ = test ()
