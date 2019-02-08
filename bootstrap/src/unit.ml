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

let compare _ _ =
  0

(*******************************************************************************
 * Begin tests.
 *)

(* XXX Add tests. *)
