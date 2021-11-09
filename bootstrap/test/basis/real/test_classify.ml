open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "%h -> %a\n" t Class.xpp (classify t);
        fn ts'
      end
  end in
  printf "@[<h>";
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ];
  printf "@]"

let _ = test ()
