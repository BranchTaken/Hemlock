open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "%f: sqrt=%f cbrt=%f\n" x (norm_nan (sqrt x)) (cbrt x);
        fn xs'
      end
  end in
  fn [-0.125; 0.; inf; 1.; 4.; 27.; 64.; 65.; 729.];
  printf "@]"

let _ = test ()
