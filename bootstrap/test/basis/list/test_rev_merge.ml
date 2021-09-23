open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let pp_pair ppf (a, b) =
    Format.fprintf ppf "(%a, %a)" Uns.pp a Uns.pp b
  in
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
  printf "@[<h>";
  iter pair_list_pairs ~f:(fun (a, b) ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    assert (is_sorted a ~cmp);
    assert (is_sorted b ~cmp);
    printf "[rev_]merge %a %a -> %a / %a\n"
      (pp pp_pair) a
      (pp pp_pair) b
      (pp pp_pair) (merge a b ~cmp)
      (pp pp_pair) (rev_merge a b ~cmp)
  );
  printf "@]"

let _ = test ()
