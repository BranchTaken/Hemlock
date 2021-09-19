open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_set len = begin
    for i = 0 to pred len do
      let arr = init len ~f:(fun _ -> 0) in
      let arr' = set i 1 arr in
      printf "set %a: %a -> %a"
        Uns.pp i
        (pp Uns.pp) arr
        (pp Uns.pp) arr'
      ;
      set_inplace i 1 arr;
      printf " -> set_inplace: %a" (pp Uns.pp) arr;
      let arr'' = copy arr in
      printf " -> copy,set_inplace: %a" (pp Uns.pp) arr'';
      set_inplace i 2 arr'';
      printf " -> %a\n"
        (pp Uns.pp) arr''
    done
  end in
  printf "@[<h>";
  test_set 1;
  test_set 2;
  test_set 3;
  printf "@]"

let _ = test ()
