module T = struct
  type t = bool

  let cmp t0 t1 =
    match t0, t1 with
    | false, true -> Cmp.Lt
    | false, false
    | true, true -> Cmp.Eq
    | true, false -> Cmp.Gt
end
include T
include Cmpable.Make_eq(T)

let of_int x =
  match x with
  | 0 -> false
  | _ -> true

let to_int t =
  match t with
  | false -> 0
  | true -> 1

let of_string s =
  match s with
  | "false" -> false
  | "true" -> true
  | _ -> assert false

let to_string t =
  match t with
  | false -> "false"
  | true -> "true"

let compare t0 t1 =
  match cmp t0 t1 with
  | Cmp.Lt -> -1
  | Cmp.Eq -> 0
  | Cmp.Gt -> 1

let sexp_of_t t =
  Sexplib.Std.sexp_of_bool t

let t_of_sexp sexp =
  Sexplib.Std.bool_of_sexp sexp

let not t =
  match t with
  | false -> true
  | true -> false

(*******************************************************************************
 * Begin tests.
 *)

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

(* XXX Add tests. *)
