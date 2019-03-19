open Rudiments

module T = struct
  type t = unit

  let hash_fold = Hash.hash_fold

  let cmp _ _ =
    Cmp.Eq

  let sexp_of_t t =
    Sexplib.Std.sexp_of_unit t

  let t_of_sexp sexp =
    Sexplib.Std.unit_of_sexp sexp

  let of_string s =
    match s with
    | "unit"
    | "()" -> ()
    | _ -> not_reached ()

  let to_string _ =
    "()"
end
include T
include Identifiable.Make(T)

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "string" =
  let open Printf in
  printf "to_string () -> %s\n" (to_string ());
  printf "of_string unit -> %s\n" (to_string (of_string "unit"));
  printf "of_string () -> %s\n" (to_string (of_string "()"));

  [%expect{|
    to_string () -> ()
    of_string unit -> ()
    of_string () -> ()
    |}]

let%expect_test "sexp" =
  let open Printf in
  let sexp = sexp_of_t () in
  printf "sexp_of_t %s -> %s ; " (to_string ()) (Sexplib.Sexp.to_string sexp);
  printf "t_of_sexp -> %s\n" (to_string (t_of_sexp sexp));

  [%expect{| sexp_of_t () -> () ; t_of_sexp -> () |}]

let%expect_test "eq" =
  let open Printf in
  let t0 = () in
  let t1 = () in
  printf "cmp %s %s -> %s\n" (to_string t0) (to_string t1)
    (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp t0 t1)));
  printf "%s = %s -> %B\n" (to_string t0) (to_string t1) (t0 = t1);
  printf "%s <> %s -> %B\n" (to_string t0) (to_string t1) (t0 <> t1);

  [%expect{|
    cmp () () -> Eq
    () = () -> true
    () <> () -> false
    |}]
