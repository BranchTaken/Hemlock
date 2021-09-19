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
    [(0, 0)];
    [(0, 0); (1, 1)];
    [(0, 0); (1, 1); (2, 2)];

    [(0, 0); (0, 1); (0, 2)];

    [(0, 0); (0, 1); (1, 2); (1, 3); (2, 4); (2, 5)];
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
