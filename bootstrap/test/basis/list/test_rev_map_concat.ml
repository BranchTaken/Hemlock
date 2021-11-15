open! Basis.Rudiments
open! Basis
open List

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
  iter list_pairs ~f:(fun (a, b) ->
    File.Fmt.stdout
    |> Fmt.fmt "rev_map_concat "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (rev_map_concat a b ~f:(fun elm -> elm + 10L))
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
