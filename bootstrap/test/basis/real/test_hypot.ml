open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec fn xys = begin
    match xys with
    | [] -> ()
    | (x, y) :: xys' -> begin
        printf "x=%f y=%f: hypot=%f\n" x y (hypot x y);
        fn xys'
      end
  end in
  fn [(3., 4.); (4., 3.); (-3., -4.);
    (-0., -3.); (0., -3.);
    (3., inf); (nan, inf); (neg_inf, nan);
  ];
  printf "@]"

let _ = test ()
