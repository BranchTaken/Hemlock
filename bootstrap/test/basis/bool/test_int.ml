open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let x = to_uns t in
        printf "to_uns %b -> %a ; " t Uns.pp x;
        printf "of_uns %a -> %b\n" Uns.pp x (of_uns x);
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
