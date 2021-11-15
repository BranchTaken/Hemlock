open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);
    ([0L; 1L], []);
    ([0L; 1L], [2L; 3L]);
    ([], [2L; 3L]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    File.Fmt.stdout
    |> Fmt.fmt "concat "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (concat a b)
    |> Fmt.fmt "\n       "
    |> (pp Uns.pp) a
    |> Fmt.fmt " @ "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (a @ b)
    |> Fmt.fmt "\nrev_concat "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (rev_concat a b)
    (* Brittle test; change in conjunction with implementation. *)
    |> Fmt.fmt "\nconcat_unordered "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (concat_unordered a b)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
