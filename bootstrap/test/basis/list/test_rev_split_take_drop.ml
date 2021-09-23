open! Basis.Rudiments
open! Basis
open! ListTest
open List
open Format

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    iter_oc 0L (succ (length l)) (fun i ->
      let a, b = split i l in
      printf "split/take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.pp i
        (pp Uns.pp) l
        (pp Uns.pp) a
        (pp Uns.pp) b
        (pp Uns.pp) (take i l)
        (pp Uns.pp) (drop i l)
      ;

      let a, b = rev_split i l in
      printf "rev_split/rev_take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.pp i
        (pp Uns.pp) l
        (pp Uns.pp) a
        (pp Uns.pp) b
        (pp Uns.pp) (rev_take i l)
        (pp Uns.pp) (drop i l)
    )
  );
  printf "@]"

let _ = test ()
