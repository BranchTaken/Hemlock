open Rudiments

module T = struct
  type t = bool

  let hash_fold = Hash.hash_fold

  let cmp t0 t1 =
    match t0, t1 with
    | false, true -> Cmp.Lt
    | false, false
    | true, true -> Cmp.Eq
    | true, false -> Cmp.Gt

  let sexp_of_t t =
    Sexplib.Std.sexp_of_bool t

  let t_of_sexp sexp =
    Sexplib.Std.bool_of_sexp sexp

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

let of_int x =
  match x with
  | 0 -> false
  | _ -> true

let to_int t =
  match t with
  | false -> 0
  | true -> 1

let compare t0 t1 =
  match cmp t0 t1 with
  | Cmp.Lt -> -1
  | Cmp.Eq -> 0
  | Cmp.Gt -> 1

let not t =
  match t with
  | false -> true
  | true -> false

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "int" =
  let open Printf in
  let rec lambda = function
    | [] -> ()
    | t :: ts' -> begin
      let x = to_int t in
      printf "to_int %b -> %d ; " t x;
      printf "of_int %d -> %b\n" x (of_int x);
      lambda ts'
    end
  in
  lambda [false; true];

  [%expect{|
    to_int false -> 0 ; of_int 0 -> false
    to_int true -> 1 ; of_int 1 -> true
    |}]

let%expect_test "string" =
  let open Printf in
  let rec lambda = function
    | [] -> ()
    | t :: ts' -> begin
      let s = to_string t in
      printf "to_string %b -> %s ; " t s;
      printf "of_string %s -> %b\n" s (of_string s);
      lambda ts'
    end
  in
  lambda [false; true];

  [%expect{|
    to_string false -> false ; of_string false -> false
    to_string true -> true ; of_string true -> true
    |}]

let%expect_test "sexp" =
  let open Printf in
  let rec lambda = function
    | [] -> ()
    | t :: ts' -> begin
        let sexp = sexp_of_t t in
        printf "sexp_of_t %b -> %s ; " t (Sexplib.Sexp.to_string sexp);
        printf "t_of_sexp -> %b\n" (t_of_sexp sexp);
        lambda ts'
      end
  in
  lambda [false; true];

  [%expect{|
    sexp_of_t false -> false ; t_of_sexp -> false
    sexp_of_t true -> true ; t_of_sexp -> true
    |}]

let%expect_test "eq" =
  let open Printf in
  let lambda t0 t1 = begin
    printf "cmp %b %b -> %s\n"
      t0 t1 (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp t0 t1)));
    printf "%b = %b -> %b\n" t0 t1 (t0 = t1);
    printf "%b <> %b -> %b\n" t0 t1 (t0 <> t1);
  end in
  lambda false false;
  printf "\n";
  lambda false true;
  printf "\n";
  lambda true false;
  printf "\n";
  lambda true true;

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
  let rec lambda bs = begin
    match bs with
    | [] -> ()
    | b :: bs' -> begin
        printf "not %b -> %b\n" b (not b);
        lambda bs'
      end
  end in
  lambda [false; true];

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
  let rec lambda pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        printf "(%b && %b) -> %b\n"
          a b ((side_effect a "a") && (side_effect b "b"));
        lambda pairs'
      end
  end in
  lambda [(false, false); (false, true); (true, false); (true, true)];

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
  let rec lambda pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        printf "(%b || %b) -> %b\n"
          a b ((side_effect a "a") || (side_effect b "b"));
        lambda pairs'
      end
  end in
  lambda [(false, false); (false, true); (true, false); (true, true)];

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
