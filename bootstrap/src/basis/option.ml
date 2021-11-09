open Rudiments0

module T = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
  type 'a elm = 'a

  let hash_fold hash_fold_a t state =
    match t with
    | None -> state |> Uns.hash_fold 0L
    | Some a -> state |> Uns.hash_fold 1L |> hash_fold_a a

  let cmp cmp_a t0 t1 =
    let open Cmp in
    match t0, t1 with
    | None, None -> Eq
    | None, Some _ -> Lt
    | Some _, None -> Gt
    | Some a0, Some a1 -> cmp_a a0 a1

  module Cursor = struct
    module T = struct
      type 'a container = 'a t
      type 'a t = {
        option: 'a container;
        index: uns;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.option == t1.option) || (Stdlib.( = ) t0.option t1.option));
        Uns.cmp t0.index t1.index

      let hd option =
        {option; index=0L}

      let tl option =
        match option with
        | None -> {option; index=0L}
        | Some _ -> {option; index=1L}

      let pred t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0L -> halt "At beginning of option"
        | Some _, _ -> begin
            assert (t.index = 1L);
            {t with index=0L}
          end

      let succ t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1L -> halt "At end of option"
        | Some _, _ -> begin
            assert (t.index = 0L);
            {t with index=1L}
          end

      let lget t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0L -> halt "At beginning of option"
        | Some v, _ -> begin
            assert (t.index = 1L);
            v
          end

      let rget t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1L -> halt "At end of option"
        | Some v, _ -> begin
            assert (t.index = 0L);
            v
          end

      let prev t =
        lget t, pred t

      let next t =
        rget t, succ t

      let container t =
        t.option

      let index t =
        t.index

      let seek i t =
        match i with
        | -1L -> pred t
        | 0L -> t
        | 1L -> succ t
        | _ -> halt "Out of bounds"
    end
    include T
    include Cmpable.MakePoly(T)
  end

  let length = function
    | None -> 0L
    | Some _ -> 1L
end
include T
include Container.MakePolyIndex(T)

let xpp xpp_a xppf = function
  | Some a -> Format.fprintf xppf "@[<h>Some@ %a@]" xpp_a a
  | None -> Format.fprintf xppf "None"

let fmt fmt_a t formatter =
  match t with
  | Some a -> formatter |> Fmt.fmt "Some " |> fmt_a a
  | None -> formatter |> Fmt.fmt "None"

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | Some _ -> false
  | None -> true

let value ~default t =
  match t with
  | Some a -> a
  | None -> default

let value_hlt = function
  | Some a -> a
  | None -> halt "No value"

let some_if b a =
  match b with
  | true -> Some a
  | false -> None

let both ta tb =
  match ta, tb with
  | Some a, Some b -> Some (a, b)
  | Some _, None
  | None, Some _
  | None, None -> None

let first_some t0 t1 =
  match t0, t1 with
  | Some a, _
  | None, Some a -> Some a
  | None, None -> None

let filter ~f t =
  match t with
  | Some a -> begin
      match f a with
      | false -> None
      | true -> Some a
    end
  | None -> None

let value_map ~default ~f t =
  match t with
  | Some a -> f a
  | None -> default

let merge ~f t0 t1 =
  match t0, t1 with
  | Some a0, Some a1 -> Some (f a0 a1)
  | Some a, None
  | None, Some a -> Some a
  | None, None -> None

let map2 ~f ta tb =
  match ta, tb with
  | Some a, Some b -> Some (f a b)
  | _, _ -> None
