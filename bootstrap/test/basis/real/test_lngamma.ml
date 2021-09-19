open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  for n = 1 to 40 do
    let n = Sint.of_int n in
    let x = (of_sint n) / 4. in
    printf "lngamma %.2f -> %.9f\n" x (lngamma x);
  done;

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "lngamma %.2f -> %.5e\n" x (lngamma x);
        fn xs'
      end
  end in
  fn [neg_inf; -1.; -0.; 0.; inf; nan];
  printf "@]"

let _ = test ()
