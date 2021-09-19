open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "gamma %.2f -> %.5e\n" x (gamma x);
        fn xs'
      end
  end in
  fn [-1.; -0.; 0.; 0.5; 10.; 171.6; 171.7; inf; nan]

let _ = test ()
