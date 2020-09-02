open Rudiments0

module T = struct
  type t = bool

  let to_uns t =
    match t with
    | false -> 0
    | true -> 1

  let hash_fold t state =
    state
    |> Uns.hash_fold (to_uns t)

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

let of_uns x =
  match x with
  | 0 -> false
  | _ -> true

let not = not

(******************************************************************************)
(* Begin tests. *)

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
    hash_fold false -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold true -> 0x3d8a_cdb4_d36d_9c06_0044_03b7_fb05_c44au128
    |}]

let%expect_test "int" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let x = to_uns t in
        printf "to_uns %b -> %a ; " t Uns.pp x;
        printf "of_uns %a -> %b\n" Uns.pp x (of_uns x);
        fn ts'
      end
  in
  fn [false; true];

  [%expect{|
    to_uns false -> 0 ; of_uns 0 -> false
    to_uns true -> 1 ; of_uns 1 -> true
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
