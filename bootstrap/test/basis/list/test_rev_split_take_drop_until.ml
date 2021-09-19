open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0];
    [0; 1];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    for i = 0 to length l do
      let f elm = (elm >= i) in
      let l0, l1 = split_until ~f l in
      printf ("split_until/take_until,drop_until %a " ^^
          "~f:(fun elm -> elm >= %a) -> %a %a / %a %a\n")
        (pp Uns.pp) l
        Uns.pp i
        (pp Uns.pp) l0
        (pp Uns.pp) l1
        (pp Uns.pp) (take_until ~f l)
        (pp Uns.pp) (drop_until ~f l)
      ;

      let rl0, rl1 = rev_split_until ~f l in
      printf ("rev_split_until/rev_take_until,drop_until %a " ^^
          "~f:(fun elm -> elm >= %a) -> %a %a / %a %a\n")
        (pp Uns.pp) l
        Uns.pp i
        (pp Uns.pp) rl0
        (pp Uns.pp) rl1
        (pp Uns.pp) (rev_take_until ~f l)
        (pp Uns.pp) (drop_until ~f l)
    done
  );
  printf "@]"

let _ = test ()
