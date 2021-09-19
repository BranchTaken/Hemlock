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
        let sx = to_sint x in
        printf "to_sint %a -> %a; of_sint -> %a\n"
          pp_x x Sint.pp_x sx pp_x (of_sint sx);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
    neg_one;
    of_sint Sint.min_value;
    of_sint Sint.max_value;
    min_value;
    max_value;
  ] in
  test_xs xs;
  printf "@]"

let _ = test ()
