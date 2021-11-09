open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0L], [1L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L; 2L], [3L; 4L; 5L]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "iter2 %a %a ->"
      (xpp Uns.xpp) a
      (xpp Uns.xpp) b
    ;
    let f i a b = begin
      printf " (i=%a, a=%a, b=%a)" Uns.xpp i Uns.xpp a Uns.xpp b
    end in
    iteri2 a b ~f;
    printf "\n"
  );
  printf "@]"

let _ = test ()
