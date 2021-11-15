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
  let pair_list_pairs = [
    ([], []);
    ([(0L, 0L)], []);
    ([], [(0L, 0L)]);

    ([(0L, 0L)], [(0L, 1L)]);
    ([(0L, 0L)], [(1L, 1L)]);
    ([(1L, 0L)], [(1L, 1L)]);

    ([(0L, 0L); (1L, 1L)], [(0L, 2L); (1L, 3L)]);
    ([(0L, 0L); (1L, 1L); (2L, 2L); (3L, 3L)], [(1L, 4L)]);
    ([(0L, 0L)], [(0L, 1L); (1L, 2L); (2L, 3L)]);
  ] in
  iter pair_list_pairs ~f:(fun (a, b) ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    assert (is_sorted a ~cmp);
    assert (is_sorted b ~cmp);
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]merge "
    |> (pp pp_pair) a
    |> Fmt.fmt " "
    |> (pp pp_pair) b
    |> Fmt.fmt " -> "
    |> (pp pp_pair) (merge a b ~cmp)
    |> Fmt.fmt " / "
    |> (pp pp_pair) (rev_merge a b ~cmp)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
