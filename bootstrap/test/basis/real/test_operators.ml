open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let norm_nan t = if (is_nan t) then nan else t in
  for i = -1 to 2 do
    let i = Sint.of_int i in
    let t0 = of_sint i in
    for j = -1 to 2 do
      let j = Sint.of_int j in
      let t1 = of_sint j in
      printf ("+ - * / %% ** copysign %.1f ~sign:%.1f -> " ^^
          "%.1f %.1f %.1f %.1f %.1f %.1f %.1f\n")
        t0 t1 (t0 + t1) (t0 - t1) (t0 * t1) (norm_nan (t0 / t1))
        (norm_nan (t0 % t1)) (t0 ** t1) (copysign ~sign:t1 t0);
    done;
    printf "~- ~+ neg abs %.1f -> %.1f %.1f %.1f %.1f\n"
      t0 (~- t0) (~+ t0) (neg t0) (abs t0);
  done;
  printf "@]"

let _ = test ()
