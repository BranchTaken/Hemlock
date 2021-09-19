open! Basis.Rudiments
open! Basis
open U63
open Format

let test () =
  printf "@[<h>";
  let rec print_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "%a, %a, %a, %a\n" pp_b x pp_o x pp x pp_x x;
        print_xs xs'
      end
  end in
  print_xs [
    min_value;
    42;
    max_value;
  ];
  printf "@]"

let _ = test ()
