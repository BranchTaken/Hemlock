open Rudiments0

module T = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
  type 'a elm = 'a

  let hash_fold hash_fold_a t state =
    match t with
    | None -> state |> Uns.hash_fold 0
    | Some a -> state |> Uns.hash_fold 1 |> hash_fold_a a

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
        {option; index=0}

      let tl option =
        match option with
        | None -> {option; index=0}
        | Some _ -> {option; index=1}

      let pred t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0 -> halt "At beginning of option"
        | Some _, _ -> begin
            assert (t.index = 1);
            {t with index=0}
          end

      let succ t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1 -> halt "At end of option"
        | Some _, _ -> begin
            assert (t.index = 0);
            {t with index=1}
          end

      let lget t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0 -> halt "At beginning of option"
        | Some v, _ -> begin
            assert (t.index = 1);
            v
          end

      let rget t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1 -> halt "At end of option"
        | Some v, _ -> begin
            assert (t.index = 0);
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
        match int_of_sint i with
        | -1 -> pred t
        | 0 -> t
        | 1 -> succ t
        | _ -> halt "Out of bounds"
    end
    include T
    include Cmpable.MakePoly(T)
  end

  let length = function
    | None -> 0
    | Some _ -> 1

  let is_empty t =
    (length t) = 0
end
include T
include ContainerCommon.MakePolyFold(T)
include ContainerArray.MakePolyArray(T)

let pp pp_a ppf = function
  | Some a -> Format.fprintf ppf "@[<h>Some@ %a@]" pp_a a
  | None -> Format.fprintf ppf "None"

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
