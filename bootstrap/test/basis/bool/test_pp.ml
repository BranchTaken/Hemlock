open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        printf "pp %b -> %a\n" t pp t;
        fn ts'
      end
  in
  printf "@[<h>";
  fn [false; true];
  printf "@]"

let _ = test ()
