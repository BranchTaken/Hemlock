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
    ([(0, 0)], []);
    ([], [(0, 0)]);

    ([(0, 0)], [(0, 1)]);
    ([(0, 0)], [(1, 1)]);
    ([(1, 0)], [(1, 1)]);

    ([(0, 0); (1, 1)], [(0, 2); (1, 3)]);
    ([(0, 0); (1, 1); (2, 2); (3, 3)], [(1, 4)]);
    ([(0, 0)], [(0, 1); (1, 2); (2, 3)]);
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
