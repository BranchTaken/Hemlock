open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let xpp_str xppf t = Format.fprintf xppf "%S" t in
  let test_map2 uarr0 uarr1 = begin
    let sarr = map2 uarr0 uarr1 ~f:(fun elm0 elm1 ->
      asprintf "(%a,%a)" Uns.xpp elm0 Uns.xpp elm1
    ) in
    let sarr2 = mapi2 uarr0 uarr1 ~f:(fun i elm0 elm1 ->
      asprintf "[%a]=(%a,%a)" Uns.xpp i Uns.xpp elm0 Uns.xpp elm1
    ) in
    printf "%a %a -> map2 %a -> mapi2 %a\n"
      (xpp Uns.xpp) uarr0
      (xpp Uns.xpp) uarr1
      (xpp xpp_str) sarr
      (xpp xpp_str) sarr2
  end in
  printf "@[<h>";
  test_map2 [||] [||];
  test_map2 [|1L|] [|0L|];
  test_map2 [|3L; 2L|] [|1L; 0L|];
  test_map2 [|5L; 4L; 3L|] [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
