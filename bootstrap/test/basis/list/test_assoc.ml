open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let assocs = [
    [];
    [(0L, 10L)];
    [(0L, 10L); (1L, 11L)];

    [(0L, 10L); (0L, 11L); (1L, 12L)];
    [(0L, 10L); (1L, 11L); (0L, 12L)];
    [(1L, 10L); (0L, 11L); (0L, 12L)];
    [(0L, 10L); (1L, 11L); (1L, 12L); (2L, 13L)];
  ] in
  let xpp_assoc xppf (a, b) = fprintf xppf "(%a,@ %a)" Uns.xpp a Uns.xpp b in
  let missing = 3L in
  let cmp = Uns.cmp in
  printf "@[<h>";
  iter assocs ~f:(fun assoc ->
    printf "%a\n"
      (xpp xpp_assoc) assoc
    ;
    iter assoc ~f:(fun (k, _) ->
      printf "find_hlt/mem %a -> %a / %b\n"
        Uns.xpp k
        Uns.xpp (Assoc.find_hlt k ~cmp assoc)
        (Assoc.mem k ~cmp assoc)
    );

    printf "find/mem %a -> " Uns.xpp missing;
    (match (Assoc.find missing ~cmp assoc),
          (Assoc.mem missing ~cmp assoc); with
      | None, b -> printf "None / %b" b
      | Some v, b -> printf "%a / %b" Uns.xpp v b
    );
    printf "\n";

    iter assoc ~f:(fun (k, _) ->
      printf "remove_hlt %a -> %a\n"
        Uns.xpp k
        (xpp xpp_assoc) (Assoc.remove_hlt k ~cmp assoc)
    );
    printf "remove %a -> %a\n"
      Uns.xpp missing
      (xpp xpp_assoc) (Assoc.remove missing ~cmp assoc)
    ;
    printf "map -> %a\n"
      (xpp xpp_assoc) (Assoc.map assoc ~f:(fun v -> v * 2L))
    ;

    printf "inverse -> %a\n"
      (xpp xpp_assoc) (Assoc.inverse assoc);

    printf "\n"
  );
  printf "@]"

let _ = test ()
