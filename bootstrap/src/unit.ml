open Rudiments

module T = struct
  type t = unit

  let hash_fold = Hash.hash_fold

  let cmp _ _ =
    Cmp.Eq

  let pp ppf _t =
    Format.fprintf ppf "()"

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

let%expect_test "pp" =
  let open Format in
  printf "@[<h>";
  printf "pp %s -> %a\n" (to_string ()) pp ();
  printf "@]";

  [%expect{| pp () -> () |}]

let%expect_test "eq" =
  let open Format in
  let t0 = () in
  let t1 = () in
  printf "@[<h>";
  printf "cmp %s %s -> %a\n" (to_string t0) (to_string t1) Cmp.pp (cmp t0 t1);
  printf "%s = %s -> %B\n" (to_string t0) (to_string t1) (t0 = t1);
  printf "%s <> %s -> %B\n" (to_string t0) (to_string t1) (t0 <> t1);
  printf "@]";

  [%expect{|
    cmp () () -> Eq
    () = () -> true
    () <> () -> false
    |}]
