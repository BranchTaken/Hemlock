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

    [(0L, 0L); (0L, 1L); (1L, 2L)];
    [(0L, 0L); (1L, 1L); (0L, 2L)];
    [(1L, 0L); (0L, 1L); (0L, 2L)];
    [(0L, 0L); (1L, 1L); (1L, 2L); (2L, 3L)];
  ] in
  iter pair_lists ~f:(fun pl ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]dedup "
    |> (pp pp_pair) pl
    |> Fmt.fmt " -> "
    |> (pp pp_pair) (dedup pl ~cmp)
    |> Fmt.fmt " / "
    |> (pp pp_pair) (rev_dedup pl ~cmp)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
