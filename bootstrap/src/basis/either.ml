open Rudiments0

type ('a, 'b) t =
  | First of 'a
  | Second of 'b

let hash_fold hash_fold_a hash_fold_b t state =
  match t with
  | First a -> state |> Uns.hash_fold 0 |> hash_fold_a a
  | Second b -> state |> Uns.hash_fold 1 |> hash_fold_b b

let cmp cmp_a cmp_b t0 t1 =
  let open Cmp in
  match t0, t1 with
  | First a0, First a1 -> cmp_a a0 a1
  | First _, Second _ -> Lt
  | Second _, First _ -> Gt
  | Second b0, Second b1 -> cmp_b b0 b1

let pp pp_a pp_b ppf = function
  | First a -> Format.fprintf ppf "@[<h>First %a@]" pp_a a
  | Second b -> Format.fprintf ppf "@[<h>Second %a@]" pp_b b

let is_first = function
  | First _ -> true
  | Second _ -> false

let is_second = function
  | First _ -> false
  | Second _ -> true

let swap = function
  | First a -> Second a
  | Second b -> First b

let value = function
  | First a
  | Second a -> a

let value_map ~first ~second = function
  | First a -> first a
  | Second b -> second b

let map ~first ~second = function
  | First a -> First (first a)
  | Second b -> Second (second b)

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold,pp" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp Uns.pp) either
          Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold Uns.hash_fold
              either Hash.State.empty));
        fn eithers'
      end
  in
  let eithers = [
    First 0;
    First 1;
    Second 0;
    Second 1;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    hash_fold First 0 -> 0x4988_6389_f40a_a2a3_3bac_4d62_a744_138bu128
    hash_fold First 1 -> 0xcffe_8f1d_4ece_31b1_0231_7d19_4ec8_ede7u128
    hash_fold Second 0 -> 0xe1d0_4b10_a3f6_4eda_dfbb_5c04_579d_0b05u128
    hash_fold Second 1 -> 0x262a_4610_b795_f867_bd59_f2c2_1134_c78cu128
    |}]

let%expect_test "is_first,is_second" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "is_first %a -> %b@\n"
          (pp Uns.pp Uns.pp) either (is_first either);
        printf "is_second %a -> %b@\n"
          (pp Uns.pp Uns.pp) either (is_second either);
        fn eithers'
      end
  in
  let eithers = [
    First 0;
    Second 0;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    is_first First 0 -> true
    is_second First 0 -> false
    is_first Second 0 -> false
    is_second Second 0 -> true
    |}]

let%expect_test "swap" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "swap %a -> %a@\n"
          (pp Uns.pp Uns.pp) either (pp Uns.pp Uns.pp) (swap either);
        fn eithers'
      end
  in
  let eithers = [
    First 0;
    Second 0;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    swap First 0 -> Second 0
    swap Second 0 -> First 0
    |}]

let%expect_test "value" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "value %a -> %a@\n"
          (pp Uns.pp Uns.pp) either Uns.pp (value either);
        fn eithers'
      end
  in
  let eithers = [
    First 0;
    Second 0;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    value First 0 -> 0
    value Second 0 -> 0
    |}]

let%expect_test "value_map" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "value_map %a -> %a@\n"
          (pp Uns.pp Uns.pp) either Uns.pp
          (value_map ~first:(fun x -> x + 2) ~second:(fun x -> x + 4) either);
        fn eithers'
      end
  in
  let eithers = [
    First 1;
    Second 2;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    value_map First 1 -> 3
    value_map Second 2 -> 6
    |}]

let%expect_test "map" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "map %a -> %a@\n"
          (pp Uns.pp Uns.pp) either (pp Uns.pp Uns.pp)
          (map ~first:(fun x -> x + 2) ~second:(fun x -> x + 4) either);
        fn eithers'
      end
  in
  let eithers = [
    First 1;
    Second 2;
  ] in
  fn eithers;
  printf "@]";

  [%expect{|
    map First 1 -> First 3
    map Second 2 -> Second 6
    |}]
