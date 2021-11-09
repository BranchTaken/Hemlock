open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let xpp_str xppf t = Format.fprintf xppf "%S" t in
  let test_map uarr = begin
    printf "%a -> map " (xpp Uns.xpp) uarr;
    let sarr = map uarr ~f:(fun elm -> asprintf "%a" Uns.xpp elm) in
    printf "%a" (xpp xpp_str) sarr;
    printf " -> mapi ";
    let sarr = mapi uarr ~f:(fun i elm ->
      asprintf "[%a]=%a" Uns.xpp i Uns.xpp elm
    ) in
    printf "%a\n" (xpp xpp_str) sarr
  end in
  printf "@[<h>";
  test_map [||];
  test_map [|0L|];
  test_map [|1L; 0L|];
  test_map [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
