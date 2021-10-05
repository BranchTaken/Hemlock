open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  for i = -1 to 1 do
    let i = Sint.extend_of_int i in
    let t0 = of_sint i in
    for j = -1 to 1 do
      let j = Sint.extend_of_int j in
      let t1 = of_sint j in
      printf "min max %.1f %.1f -> %.1f %.1f\n" t0 t1 (min t0 t1) (max t0 t1);
    done;
  done;
  printf "@]"

let _ = test ()
