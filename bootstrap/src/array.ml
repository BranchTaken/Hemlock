module Int = I63
open Rudiments_functions

type 'a t = 'a array

let empty = [||]

module Seq = struct
  type 'a outer = 'a t
  module type S_mono = sig
    type t
    type elm
    val to_array: t -> elm outer
  end
  module type S_poly = sig
    type 'a t
    type 'a elm
    val to_array: 'a t -> 'a elm outer
  end

  module Make_mono (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                       and type elm := T.elm =
  struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> empty
      | _ -> begin
          let rec fn t a i = begin
            match Int.(i = l) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module Make_mono_rev (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                           and type elm := T.elm
  = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> empty
      | _ -> begin
          let rec fn t a i = begin
            match Int.(i = 0) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let i' = pred i in
                let () = Stdlib.Array.set a i' elm in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          fn t' a (pred l)
        end
  end

  module Make_poly (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> empty
      | _ -> begin
          let rec fn t a i = begin
            match Int.(i = l) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module Make_poly_rev (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> empty
      | _ -> begin
          let rec fn t a i = begin
            match Int.(i = 0) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let i' = pred i in
                let () = Stdlib.Array.set a i' elm in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          fn t' a (pred l)
        end
  end
end

let init n ~f =
  Stdlib.Array.init n f

let of_list l =
  (* XXX Use Seq.Make. *)
  Stdlib.Array.of_list l

let of_list_rev l =
  (* XXX Use Seq.Make_rev. *)
  Stdlib.Array.of_list (List.rev l)

let to_list a =
  (* XXX Use fold_right. *)
  Stdlib.Array.to_list a

let to_list_rev a =
  (* XXX Use fold. *)
  List.rev (Stdlib.Array.to_list a)

let length a =
  Stdlib.Array.length a

let is_empty a =
  (length a) = 0

let get a i =
  Stdlib.Array.get a i

let mutate a i x =
  Stdlib.Array.set a i x

let copy a =
  (* XXX Use Seq.Make. *)
  Stdlib.Array.copy a

let slice a start stop =
  (* XXX Use Seq.Make. *)
  Stdlib.Array.sub a start stop

let set a i x =
  let v' = copy a in
  mutate v' i x;
  v'

let concat al =
  (* XXX Use Seq.Make. *)
  Stdlib.Array.concat al

let append a0 a1 =
  concat [a0; a1]

let append_one a x =
  append a (of_list [x])

let insert a i x =
  let len = length a in
  if Int.(i = 0) then
    append (of_list [x]) a
  else if Int.(i < len) then
    concat [(slice a 0 i); (of_list [x]); (slice a i len)]
  else
    append a (of_list [x])

let remove a i =
  let len = length a in
  match len with
  | 0 ->
    ignore (assert (i = 0));
    empty
  | _ -> begin
      if Int.(i = 0) then
        slice a 1 len
      else if Int.(i < len) then
        concat [(slice a 0 i); (slice a (i + 1) len)]
      else
        slice a 0 (len - 1)
    end

(* XXX Rewrite without Stdlib dependency. *)
let iter a ~f =
  Stdlib.Array.iter f a

(* XXX Rewrite without Stdlib dependency. *)
let iteri a ~f =
  Stdlib.Array.iteri f a

(* XXX Rewrite based on fold_until. *)
let fold a ~init ~f =
  Stdlib.Array.fold_left f init a

(* XXX Create fold_right_until, rewrite fold_right to use it. *)
let fold_right a ~init ~f =
  Stdlib.Array.fold_right f a init

let foldi a ~init ~f =
  let accum, _ = fold a ~init:(init, 0) ~f:(fun (accum, i) v ->
    let accum' = f i accum v in
    (accum', i + 1)
  ) in
  accum

let foldi_until a ~init ~f =
  let rec fn accum i = begin
    match Int.( = ) i (length a) with
    | true -> accum
    | false -> begin
        let elm = get a i in
        let (accum', until) = f i accum elm in
        match until with
        | true -> accum'
        | false -> fn accum' (i + 1)
      end
  end in
  fn init 0

let fold_until a ~init ~f =
  let rec fn accum i = begin
    match Int.( = ) i (length a) with
    | true -> accum
    | false -> begin
        let elm = get a i in
        let (accum', until) = f accum elm in
        match until with
        | true -> accum'
        | false -> fn accum' (i + 1)
      end
  end in
  fn init 0

let compare cmp a0 a1 =
  let rel = foldi_until a0 ~init:0 ~f:(fun i _ elm ->
    match Int.( <= ) (length a1) i with
    | true -> 1, true
    | false -> begin
        let rel = cmp elm (get a1 i) in
        match rel with
        | -1 | 1 -> rel, true
        | 0 -> 0, false
        | _ -> not_reached ()
      end
  ) in
  match rel with
  | -1 | 1 -> rel
  | 0 -> compare (length a0) (length a1)
  | _ -> not_reached ()

let sexp_of_t t =
  Sexplib.Std.sexp_of_array t

let t_of_sexp sexp =
  Sexplib.Std.array_of_sexp sexp

(*******************************************************************************
 * Begin tests.
 *)

(* XXX Add tests. *)
