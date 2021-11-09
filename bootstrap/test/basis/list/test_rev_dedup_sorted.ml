open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let xpp_pair xppf (a, b) =
    Format.fprintf xppf "(%a, %a)" Uns.xpp a Uns.xpp b
  in
  let pair_lists = [
    [];
    [(0L, 0L)];
    [(0L, 0L); (1L, 1L)];
    [(0L, 0L); (1L, 1L); (2L, 2L)];

    [(0L, 0L); (0L, 1L); (0L, 2L)];

    [(0L, 0L); (0L, 1L); (1L, 2L); (1L, 3L); (2L, 4L); (2L, 5L)];
  ] in
  printf "@[<h>";
  iter pair_lists ~f:(fun pl ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    assert (is_sorted pl ~cmp);
    printf "[rev_]dedup_sorted %a -> %a / %a\n"
      (xpp xpp_pair) pl
      (xpp xpp_pair) (dedup_sorted pl ~cmp)
      (xpp xpp_pair) (rev_dedup_sorted pl ~cmp)
    ;
  );
  printf "@]"

let _ = test ()
