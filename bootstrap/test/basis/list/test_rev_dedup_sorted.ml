open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let pp_pair ppf (a, b) =
    Format.fprintf ppf "(%a, %a)" Uns.pp a Uns.pp b
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
      (pp pp_pair) pl
      (pp pp_pair) (dedup_sorted pl ~cmp)
      (pp pp_pair) (rev_dedup_sorted pl ~cmp)
    ;
  );
  printf "@]"

let _ = test ()
