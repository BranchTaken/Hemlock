open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let xpp_str xppf t = Format.fprintf xppf "%S" t in
  let test_fold_map uarr = begin
    let accum, sarr = fold_map uarr ~init:0L ~f:(fun accum elm ->
      (accum + elm), (asprintf "%a" Uns.xpp elm)
    ) in
    let accum2, sarr2 = foldi_map uarr ~init:0L ~f:(fun i accum elm ->
      (accum + i + elm),
      (asprintf "[%a]=%a" Uns.xpp i Uns.xpp elm)
    ) in
    printf "%a -> fold_map %a %a -> foldi_map %a %a\n"
      (xpp Uns.xpp) uarr
      Uns.xpp accum
      (xpp xpp_str) sarr
      Uns.xpp accum2
      (xpp xpp_str) sarr2
  end in
  printf "@[<h>";
  test_fold_map [||];
  test_fold_map [|0L|];
  test_fold_map [|1L; 0L|];
  test_fold_map [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
