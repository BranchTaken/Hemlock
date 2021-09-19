open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_swap arr = begin
    for i = 0 to pred (length arr) do
      for j = i to pred (length arr) do
        let arr' = copy arr in
        printf "%a %a: swap %a -> %a -> swap_inplace %a -> "
          Uns.pp i
          Uns.pp j
          (pp Uns.pp) arr'
          (pp Uns.pp) (swap i j arr')
          (pp Uns.pp) arr'
        ;
        swap_inplace i j arr';
        printf "%a\n" (pp Uns.pp) arr'
      done
    done
  end in
  printf "@[<h>";
  test_swap [|0|];
  test_swap [|0; 1|];
  test_swap [|0; 1; 2|];
  test_swap [|0; 1; 2; 3|];
  printf "@]"

let _ = test ()
