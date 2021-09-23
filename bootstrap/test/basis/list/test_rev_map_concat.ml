open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0L; 1L], []);
    ([0L], [1L]);
    ([], [0L; 1L]);

    ([0L; 1L; 2L], []);
    ([0L; 1L], [2L]);
    ([0L], [1L; 2L]);
    ([], [1L; 2L; 3L]);

    ([0L; 1L; 2L; 3L], []);
    ([0L; 1L; 2L], [3L]);
    ([0L; 1L], [2L; 3L]);
    ([0L], [1L; 2L; 3L]);
    ([], [0L; 1L; 2L; 3L])
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "rev_map_concat %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_map_concat a b ~f:(fun elm -> elm + 10L))
  );
  printf "@]"

let _ = test ()
