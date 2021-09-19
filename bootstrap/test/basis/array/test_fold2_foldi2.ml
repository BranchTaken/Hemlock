open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_fold2 uarr0 uarr1 = begin
    printf "%a %a"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
    ;
    let accum = fold2 uarr0 uarr1 ~init:0 ~f:(fun accum elm0 elm1 ->
      accum + elm0 + elm1
    ) in
    printf " -> fold2 %a" Uns.pp accum;
    let accum = foldi2 uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 -> accum + i + elm0 + elm1 ) in
    printf " -> foldi2 %a\n" Uns.pp accum
  end in
  printf "@[<h>";
  test_fold2 [||] [||];
  test_fold2 [|1|] [|0|];
  test_fold2 [|3; 2|] [|1; 0|];
  test_fold2 [|5; 4; 3|] [|2; 1; 0|];
  printf "@]"

let _ = test ()
