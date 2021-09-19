open! Basis.Rudiments
open! Basis
open N
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    zero; (* No bits, therefore the bitwise not is also no bits. *)
    one;
    of_u64 U64.max_value
  ] in
  test xs;
  printf "@]"

let _ = test ()
