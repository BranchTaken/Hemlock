open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_fold2_map uarr0 uarr1 = begin
    let accum, sarr = fold2_map uarr0 uarr1 ~init:0L
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1),
          (asprintf "(%a,%a)" Uns.pp elm0 Uns.pp elm1)
        ) in
    let accum2, sarr2 = foldi2_map uarr0 uarr1 ~init:0L
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1),
          (asprintf "[%a]=(%a,%a)" Uns.pp i Uns.pp elm0 Uns.pp elm1)
        ) in
    printf "%a %a -> fold2_map %a %a -> foldi2_map %a %a\n"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
      Uns.pp accum
      (pp pp_str) sarr
      Uns.pp accum2
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_fold2_map [||] [||];
  test_fold2_map [|1L|] [|0L|];
  test_fold2_map [|3L; 2L|] [|1L; 0L|];
  test_fold2_map [|5L; 4L; 3L|] [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
