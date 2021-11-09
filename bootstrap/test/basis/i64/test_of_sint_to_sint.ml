open! Basis.Rudiments
open! Basis
open I64
open Format

let test () =
  printf "@[<h>";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let sx = x in
        printf "to_sint %a -> %a; of_sint -> %a\n"
          xpp_x x Sint.xpp_x sx xpp_x sx;
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
    neg_one;
    Sint.min_value;
    Sint.max_value;
    min_value;
    max_value;
  ] in
  test_xs xs;
  printf "@]"

let _ = test ()
