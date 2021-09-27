open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_fold2_until uarr0 uarr1 = begin
    printf "%a %a"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
    ;
    let accum = fold2_until uarr0 uarr1 ~init:0
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1), (accum > 10)
        ) in
    printf " -> fold2_until %a" Uns.pp accum;
    let accum = foldi2_until uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1), ((i + 2) >= (length uarr0))
        ) in
    printf " -> foldi2_until %a\n" Uns.pp accum
  end in
  printf "@[<h>";
  test_fold2_until [||] [||];
  test_fold2_until [|1|] [|0|];
  test_fold2_until [|3; 2|] [|1; 0|];
  test_fold2_until [|5; 4; 3|] [|2; 1; 0|];
  printf "@]"

let _ = test ()