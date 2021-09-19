open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0; 1], []);
    ([0], [1]);
    ([], [0; 1]);

    ([0; 1; 2], []);
    ([0; 1], [2]);
    ([0], [1; 2]);
    ([], [1; 2; 3]);

    ([0; 1; 2; 3], []);
    ([0; 1; 2], [3]);
    ([0; 1], [2; 3]);
    ([0], [1; 2; 3]);
    ([], [0; 1; 2; 3])
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "rev_map_concat %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_map_concat a b ~f:(fun elm -> elm + 10))
  );
  printf "@]"

let _ = test ()
