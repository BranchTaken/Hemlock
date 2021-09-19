open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  let rec fn bs = begin
    match bs with
    | [] -> ()
    | b :: bs' -> begin
        printf "not %b -> %b\n" b (not b);
        fn bs'
      end
  end in
  fn [false; true]

let _ = test ()
