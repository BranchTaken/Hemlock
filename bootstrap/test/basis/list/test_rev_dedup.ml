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

    [(0, 0); (0, 1); (1, 2)];
    [(0, 0); (1, 1); (0, 2)];
    [(1, 0); (0, 1); (0, 2)];
    [(0, 0); (1, 1); (1, 2); (2, 3)];
  ] in
  printf "@[<h>";
  iter pair_lists ~f:(fun pl ->
    let cmp (a, _) (b, _) = Uns.cmp a b in
    printf "[rev_]dedup %a -> %a / %a\n"
      (pp pp_pair) pl
      (pp pp_pair) (dedup pl ~cmp)
      (pp pp_pair) (rev_dedup pl ~cmp)
  );
  printf "@]"

let _ = test ()
