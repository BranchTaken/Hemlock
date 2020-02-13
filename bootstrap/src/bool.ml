open Rudiments

module T = struct
  type t = bool

  let to_usize t =
    match t with
    | false -> 0
    | true -> 1

  let hash_fold t state =
    state
    |> Usize.hash_fold (to_usize t)

  let cmp t0 t1 =
    match t0, t1 with
    | false, true -> Cmp.Lt
    | false, false
    | true, true -> Cmp.Eq
    | true, false -> Cmp.Gt

  let pp ppf t =
    Format.fprintf ppf (
      match t with
      | false -> "false"
      | true -> "true"
    )

  let of_string s =
    match s with
    | "false" -> false
    | "true" -> true
    | _ -> not_reached ()

  let to_string t =
    match t with
    | false -> "false"
    | true -> "true"
end
include T
include Identifiable.Make(T)

let of_usize x =
  match x with
  | 0 -> false
  | _ -> true

let not t =
  match t with
  | false -> true
  | true -> false

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold bools = begin
    match bools with
    | [] -> ()
    | b :: bools' -> begin
        printf "hash_fold %a -> %a\n"
          pp b Hash.pp (Hash.t_of_state (hash_fold b Hash.State.empty));
        test_hash_fold bools'
      end
  end in
  let bools = [false; true] in
  test_hash_fold bools;
  printf "@]";

  [%expect{|
    hash_fold false -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold true -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    |}]

let%expect_test "int" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
      let x = to_usize t in
      printf "to_usize %b -> %a ; " t Usize.pp x;
      printf "of_usize %a -> %b\n" Usize.pp x (of_usize x);
      fn ts'
    end
  in
  fn [false; true];

  [%expect{|
    to_usize false -> 0 ; of_usize 0 -> false
    to_usize true -> 1 ; of_usize 1 -> true
    |}]

let%expect_test "string" =
  let open Printf in
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
      let s = to_string t in
      printf "to_string %b -> %s ; " t s;
      printf "of_string %s -> %b\n" s (of_string s);
      fn ts'
    end
  in
  fn [false; true];

  [%expect{|
    to_string false -> false ; of_string false -> false
    to_string true -> true ; of_string true -> true
    |}]

let%expect_test "pp" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        printf "pp %b -> %a\n" t pp t;
        fn ts'
      end
  in
  printf "@[<h>";
  fn [false; true];
  printf "@]";

  [%expect{|
    pp false -> false
    pp true -> true
    |}]

let%expect_test "eq" =
  let open Format in
  let fn t0 t1 = begin
    printf "cmp %b %b -> %a\n" t0 t1 Cmp.pp (cmp t0 t1);
    printf "%b = %b -> %b\n" t0 t1 (t0 = t1);
    printf "%b <> %b -> %b\n" t0 t1 (t0 <> t1);
  end in
  printf "@[<h>";
  fn false false;
  printf "\n";
  fn false true;
  printf "\n";
  fn true false;
  printf "\n";
  fn true true;
  printf "@]";

  [%expect{|
    cmp false false -> Eq
    false = false -> true
    false <> false -> false

    cmp false true -> Lt
    false = true -> false
    false <> true -> true

    cmp true false -> Gt
    true = false -> false
    true <> false -> true

    cmp true true -> Eq
    true = true -> true
    true <> true -> false |}]

let%expect_test "not" =
  let open Printf in
  let rec fn bs = begin
    match bs with
    | [] -> ()
    | b :: bs' -> begin
        printf "not %b -> %b\n" b (not b);
        fn bs'
      end
  end in
  fn [false; true];

  [%expect{|
    not false -> true
    not true -> false
    |}]

let%expect_test "and" =
  let open Printf in
  let side_effect b s = begin
    printf "side effect %s\n" s;
    b
  end in
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        printf "(%b && %b) -> %b\n"
          a b ((side_effect a "a") && (side_effect b "b"));
        fn pairs'
      end
  end in
  fn [(false, false); (false, true); (true, false); (true, true)];

  [%expect{|
    side effect a
    (false && false) -> false
    side effect a
    (false && true) -> false
    side effect a
    side effect b
    (true && false) -> false
    side effect a
    side effect b
    (true && true) -> true
    |}]

let%expect_test "or" =
  let open Printf in
  let side_effect b s = begin
    printf "side effect %s\n" s;
    b
  end in
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        printf "(%b || %b) -> %b\n"
          a b ((side_effect a "a") || (side_effect b "b"));
        fn pairs'
      end
  end in
  fn [(false, false); (false, true); (true, false); (true, true)];

  [%expect{|
    side effect a
    side effect b
    (false || false) -> false
    side effect a
    side effect b
    (false || true) -> true
    side effect a
    (true || false) -> true
    side effect a
    (true || true) -> true
    |}]
