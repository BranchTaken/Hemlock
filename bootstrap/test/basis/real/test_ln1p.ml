open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "ln,ln1p %h -> %h %h\n" t (norm_nan (ln (1. + t)))
          (norm_nan (ln1p t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -2.; -1.; 1.;
    (ex 1.); (ex 2.);
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ];
  printf "@]"

let _ = test ()
