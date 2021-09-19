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
  let f i a b = (b + a + i + 1) in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "    mapi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (mapi2 a b ~f)
    ;

    printf "rev_mapi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (rev_mapi2 a b ~f)
  );
  printf "@]"

let _ = test ()
