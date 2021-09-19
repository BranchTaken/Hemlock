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
        printf "log %h -> %h\n" t (norm_nan (log t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    10.; 100.;
    -0.; 0.;
  ];
  printf "@]"

let _ = test ()
