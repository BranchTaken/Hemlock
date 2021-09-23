open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([10L], [100L]);
    ([10L; 20L], [100L; 200L]);
    ([10L; 20L; 30L], [100L; 200L; 300L]);
  ] in
  let f i accum a b = begin
    let sum = (b + a + i + 1L) in
    (accum + sum), sum
  end in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let accum, c = foldi2_map a b ~init:0L ~f in
    printf "    foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
    ;

    let accum, c = rev_foldi2_map a b ~init:0L ~f in
    printf "rev_foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
  );
  printf "@]"

let _ = test ()
