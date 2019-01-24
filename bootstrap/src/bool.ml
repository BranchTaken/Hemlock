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

(*******************************************************************************
 * Begin tests.
 *)

(* XXX Add tests. *)
