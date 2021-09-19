open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_filter arr = begin
    let farr = filter arr ~f:(fun elm -> elm % 2 = 0) in
    let farr2 = filteri arr ~f:(fun _ elm -> elm % 2 = 0) in
    let farr3 = filteri arr ~f:(fun i _ -> i % 2 = 0) in
    printf "%a -> filter %a -> filteri %a %a\n"
      (pp Uns.pp) arr
      (pp Uns.pp) farr
      (pp Uns.pp) farr2
      (pp Uns.pp) farr3
  end in
  printf "@[<h>";
  test_filter [||];
  test_filter [|0|];
  test_filter [|1; 0|];
  test_filter [|2; 1; 0|];
  test_filter [|3; 2; 1; 0|];
  printf "@]"

let _ = test ()
