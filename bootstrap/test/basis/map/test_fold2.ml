open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[";
  let xpp_pair xppf (kv0_opt, kv1_opt) = begin
    fprintf xppf "(%a, %a)"
      (Option.xpp (xpp_kv Uns.xpp)) kv0_opt
      (Option.xpp (xpp_kv Uns.xpp)) kv1_opt
  end in
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let pairs = fold2 ~init:[] ~f:(fun accum kv0_opt kv1_opt ->
      (kv0_opt, kv1_opt) :: accum
    ) map0 map1 in
    printf "fold2 %a %a -> %a@\n"
      (List.xpp Uns.xpp) ks0
      (List.xpp Uns.xpp) ks1
      (List.xpp xpp_pair) pairs
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
    [42L; 420L];
    [42L; 420L; 421L];
    [42L; 420L; 4200L];
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then test ks0 ks1
    )
  )

let _ = test ()
