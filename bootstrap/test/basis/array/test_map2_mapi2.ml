open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_map2 uarr0 uarr1 = begin
    let sarr = map2 uarr0 uarr1 ~f:(fun elm0 elm1 ->
      asprintf "(%a,%a)" Uns.pp elm0 Uns.pp elm1
    ) in
    let sarr2 = mapi2 uarr0 uarr1 ~f:(fun i elm0 elm1 ->
      asprintf "[%a]=(%a,%a)" Uns.pp i Uns.pp elm0 Uns.pp elm1
    ) in
    printf "%a %a -> map2 %a -> mapi2 %a\n"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
      (pp pp_str) sarr
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_map2 [||] [||];
  test_map2 [|1L|] [|0L|];
  test_map2 [|3L; 2L|] [|1L; 0L|];
  test_map2 [|5L; 4L; 3L|] [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
