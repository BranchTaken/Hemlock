open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);

    ([100], [200]);
    ([100; 110], [200; 210]);
    ([100; 110; 120], [200; 210; 220]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let f i accum a_elm b_elm = begin
      let len = length a in
      let limit = len - 2 in
      ((i + a_elm + b_elm) :: accum), (i = limit)
    end in
    printf "foldi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (foldi2_until a b ~init:[] ~f)
  );
  printf "@]"

let _ = test ()
