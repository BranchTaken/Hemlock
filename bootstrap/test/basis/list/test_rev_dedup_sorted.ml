open! Basis.Rudiments
open! Basis
open List

let test () =
  let pp_pair (a, b) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> Uns.pp a
    |> Fmt.fmt ", "
    |> Uns.pp b
    |> Fmt.fmt ")"
  end in
  let pair_lists = [
    [];
    [(0L, 0L)];
    [(0L, 0L); (1L, 1L)];
    [(0L, 0L); (1L, 1L); (2L, 2L)];

    [(0L, 0L); (0L, 1L); (0L, 2L)];

    [(0L, 0L); (0L, 1L); (1L, 2L); (1L, 3L); (2L, 4L); (2L, 5L)];
  ] in
  iter pair_lists ~f:(fun pl ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    assert (is_sorted pl ~cmp);
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]dedup_sorted "
    |> (pp pp_pair) pl
    |> Fmt.fmt " -> "
    |> (pp pp_pair) (dedup_sorted pl ~cmp)
    |> Fmt.fmt " / "
    |> (pp pp_pair) (rev_dedup_sorted pl ~cmp)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
