open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let assocs = [
    [];
    [(0, 10)];
    [(0, 10); (1, 11)];

    [(0, 10); (0, 11); (1, 12)];
    [(0, 10); (1, 11); (0, 12)];
    [(1, 10); (0, 11); (0, 12)];
    [(0, 10); (1, 11); (1, 12); (2, 13)];
  ] in
  let pp_assoc ppf (a, b) = fprintf ppf "(%a,@ %a)" Uns.pp a Uns.pp b in
  let missing = 3 in
  let cmp = Uns.cmp in
  printf "@[<h>";
  iter assocs ~f:(fun assoc ->
    printf "%a\n"
      (pp pp_assoc) assoc
    ;
    iter assoc ~f:(fun (k, _) ->
      printf "find_hlt/mem %a -> %a / %b\n"
        Uns.pp k
        Uns.pp (Assoc.find_hlt k ~cmp assoc)
        (Assoc.mem k ~cmp assoc)
    );

    printf "find/mem %a -> " Uns.pp missing;
    (match (Assoc.find missing ~cmp assoc),
          (Assoc.mem missing ~cmp assoc); with
      | None, b -> printf "None / %b" b
      | Some v, b -> printf "%a / %b" Uns.pp v b
    );
    printf "\n";

    iter assoc ~f:(fun (k, _) ->
      printf "remove_hlt %a -> %a\n"
        Uns.pp k
        (pp pp_assoc) (Assoc.remove_hlt k ~cmp assoc)
    );
    printf "remove %a -> %a\n"
      Uns.pp missing
      (pp pp_assoc) (Assoc.remove missing ~cmp assoc)
    ;
    printf "map -> %a\n"
      (pp pp_assoc) (Assoc.map assoc ~f:(fun v -> v * 2))
    ;

    printf "inverse -> %a\n"
      (pp pp_assoc) (Assoc.inverse assoc);

    printf "\n"
  );
  printf "@]"

let _ = test ()
