open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([10], [100]);
    ([10; 20], [100; 200]);
    ([10; 20; 30], [100; 200; 300]);
  ] in
  let f i accum a b = begin
    let sum = (b + a + i + 1) in
    (accum + sum), sum
  end in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let accum, c = foldi2_map a b ~init:0 ~f in
    printf "    foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
    ;

    let accum, c = rev_foldi2_map a b ~init:0 ~f in
    printf "rev_foldi2_map %a %a -> %a, %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      Uns.pp accum
      (pp Uns.pp) c
  );
  printf "@]"

let _ = test ()
