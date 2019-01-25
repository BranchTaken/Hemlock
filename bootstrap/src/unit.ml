type t = unit

let cmp _ _ =
  Cmp.Eq

let of_string s =
  match s with
  | "unit"
  | "()" -> ()
  | _ -> assert false

let to_string _ =
  "()"

let compare _ _ =
  0

let sexp_of_t t =
  Sexplib.Std.sexp_of_unit t

let t_of_sexp sexp =
  Sexplib.Std.unit_of_sexp sexp

(*******************************************************************************
 * Begin tests.
 *)

(* XXX Add tests. *)
