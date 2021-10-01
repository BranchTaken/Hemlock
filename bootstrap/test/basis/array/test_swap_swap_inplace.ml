open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_swap arr = begin
    Range.iter (0L =:< (length arr)) ~f:(fun i ->
      Range.iter (i =:< (length arr)) ~f:(fun j ->
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
      )
    )
  end in
  printf "@[<h>";
  test_swap [|0L|];
  test_swap [|0L; 1L|];
  test_swap [|0L; 1L; 2L|];
  test_swap [|0L; 1L; 2L; 3L|];
  printf "@]"

let _ = test ()
