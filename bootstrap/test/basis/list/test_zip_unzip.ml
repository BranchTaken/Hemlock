open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0], [1]);
    ([0; 1], [2; 3]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  let pp_pair ppf (a, b) = fprintf ppf "(%a,@ %a)" Uns.pp a Uns.pp b in
  printf "@[<h>";
  iter list_pairs ~f:(fun (t0, t1) ->
    let z = zip t0 t1 in
    let t0', t1' = unzip z in
    printf "zip/unzip %a %a -> %a -> %a %a\n"
      (pp Uns.pp) t0
      (pp Uns.pp) t1
      (pp pp_pair) z
      (pp Uns.pp) t0'
      (pp Uns.pp) t1'
  );
  printf "@]"

let _ = test ()
