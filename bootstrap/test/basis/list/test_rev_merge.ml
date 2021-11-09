open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let xpp_pair xppf (a, b) =
    Format.fprintf xppf "(%a, %a)" Uns.xpp a Uns.xpp b
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
      (xpp xpp_pair) a
      (xpp xpp_pair) b
      (xpp xpp_pair) (merge a b ~cmp)
      (xpp xpp_pair) (rev_merge a b ~cmp)
  );
  printf "@]"

let _ = test ()
