open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_filter arr = begin
    let farr = filter arr ~f:(fun elm -> elm % 2L = 0L) in
    let farr2 = filteri arr ~f:(fun _ elm -> elm % 2L = 0L) in
    let farr3 = filteri arr ~f:(fun i _ -> i % 2L = 0L) in
    printf "%a -> filter %a -> filteri %a %a\n"
      (pp Uns.pp) arr
      (pp Uns.pp) farr
      (pp Uns.pp) farr2
      (pp Uns.pp) farr3
  end in
  printf "@[<h>";
  test_filter [||];
  test_filter [|0L|];
  test_filter [|1L; 0L|];
  test_filter [|2L; 1L; 0L|];
  test_filter [|3L; 2L; 1L; 0L|];
  printf "@]"

let _ = test ()
