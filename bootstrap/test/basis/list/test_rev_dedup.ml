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

    [(0L, 0L); (0L, 1L); (1L, 2L)];
    [(0L, 0L); (1L, 1L); (0L, 2L)];
    [(1L, 0L); (0L, 1L); (0L, 2L)];
    [(0L, 0L); (1L, 1L); (1L, 2L); (2L, 3L)];
  ] in
  printf "@[<h>";
  iter pair_lists ~f:(fun pl ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    printf "[rev_]dedup %a -> %a / %a\n"
      (xpp xpp_pair) pl
      (xpp xpp_pair) (dedup pl ~cmp)
      (xpp xpp_pair) (rev_dedup pl ~cmp)
  );
  printf "@]"

let _ = test ()
