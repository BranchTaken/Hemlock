open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0L], [1L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L; 2L], [3L; 4L; 5L])
  ] in
  let xpp_pair xppf (a, b) = fprintf xppf "(%a,@ %a)" Uns.xpp a Uns.xpp b in
  printf "@[<h>";
  iter list_pairs ~f:(fun (t0, t1) ->
    let z = zip t0 t1 in
    let t0', t1' = unzip z in
    printf "zip/unzip %a %a -> %a -> %a %a\n"
      (xpp Uns.xpp) t0
      (xpp Uns.xpp) t1
      (xpp xpp_pair) z
      (xpp Uns.xpp) t0'
      (xpp Uns.xpp) t1'
  );
  printf "@]"

let _ = test ()
