open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let ns = [kv (-1); kv 0; kv 1] in
  let rec fn ns = begin
    match ns with
    | [] -> ()
    | n :: ns' -> begin
        printf "%a\n" pp n;
        printf "  is_positive    =%b\n" (is_positive n);
        printf "  is_non_negative=%b\n" (is_non_negative n);
        printf "  is_negative    =%b\n" (is_negative n);
        printf "  is_non_positive=%b\n" (is_non_positive n);
        fn ns'
      end
  end in
  fn ns

let _ = test ()
