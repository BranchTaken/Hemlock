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
  let f i accum a_elm b_elm = begin
    (i + a_elm + b_elm) :: accum
  end in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "foldi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (foldi2 a b ~init:[] ~f)
    ;
  );
  printf "@]"

let _ = test ()
