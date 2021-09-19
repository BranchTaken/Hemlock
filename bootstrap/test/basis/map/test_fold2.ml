open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[";
  let pp_pair ppf (kv0_opt, kv1_opt) = begin
    fprintf ppf "(%a, %a)"
      (Option.pp (pp_kv Uns.pp)) kv0_opt
      (Option.pp (pp_kv Uns.pp)) kv1_opt
  end in
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let pairs = fold2 ~init:[] ~f:(fun accum kv0_opt kv1_opt ->
      (kv0_opt, kv1_opt) :: accum
    ) map0 map1 in
    printf "fold2 %a %a -> %a@\n"
      (List.pp Uns.pp) ks0
      (List.pp Uns.pp) ks1
      (List.pp pp_pair) pairs
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then test ks0 ks1
    )
  )

let _ = test ()
