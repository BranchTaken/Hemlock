open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let s = to_string t in
        printf "to_string %b -> %s ; " t s;
        printf "of_string %s -> %b\n" s (of_string s);
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
