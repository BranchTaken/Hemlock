type ('a, 'cmp) t = ('a, unit, 'cmp) Map.t

type ('a, 'cmp) cmper = ('a, 'cmp) Map.cmper

let cmper_m = Map.cmper_m

let cmper = Map.cmper

let empty = Map.empty

let singleton m a =
  Map.singleton m ~k:a ~v:()

let length = Map.length

let is_empty = Map.is_empty

let mem = Map.mem

let choose t =
  match Map.choose t with
  | Some (k, _) -> Some k
  | None -> None

let choose_hlt t =
  let k, _ = Map.choose_hlt t in
  k

let insert a t =
  Map.insert ~k:a ~v:() t

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist set = begin
        match alist with
        | [] -> set
        | a :: alist' -> fn alist' (insert a set)
      end in
      fn alist' (singleton m a)
    end

let remove = Map.remove

let fold_until ~init ~f t =
  Map.fold_until ~init ~f:(fun accum (k, _) -> f accum k) t

let fold ~init ~f t =
  fold_until ~init ~f:(fun accum a -> (f accum a), false) t

let iter ~f t =
  fold ~init:() ~f:(fun _ a -> f a) t

let hash_fold t state =
  Map.hash_fold Unit.hash_fold t state

(* Seq. *)
module SeqPoly2Fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type ('a, 'cmp) t = ('a, unit, 'cmp) Map.Seq.t

  let init = Map.Seq.init

  let length = Map.Seq.length

  let next t =
    let (a, _), t' = Map.Seq.next t in
    a, t'

  let next_opt t =
    match Map.Seq.next_opt t with
    | Some ((a, _), t') -> Some (a, t')
    | None -> None

  let cmper = Map.Seq.cmper

  let cmp = Map.Seq.cmp
end
include Seq.MakePoly2Fold2(SeqPoly2Fold2)
module Seq = SeqPoly2Fold2

let equal t0 t1 =
  Map.equal Unit.( = ) t0 t1

let subset t0 t1 =
  Map.subset Unit.( = ) t0 t1

let disjoint t0 t1 =
  Map.disjoint t0 t1

let union t0 t1 =
  Map.union ~f:(fun _ _ _ -> ()) t0 t1

let inter t0 t1 =
  Map.inter ~f:(fun _ _ _ -> ()) t0 t1

let diff = Map.diff

let count ~f t =
  Map.count ~f:(fun (a, _) -> f a) t

let for_any ~f t =
  Map.for_any ~f:(fun (a, _) -> f a) t

let for_all ~f t =
  Map.for_all ~f:(fun (a, _) -> f a) t

let find ~f t =
  match Map.find ~f:(fun (a, _) -> f a) t with
  | Some (a, _) -> Some a
  | None -> None

let find_map ~f t =
  Map.find_map ~f:(fun (a, _) -> f a) t

let filter ~f t =
  Map.filter ~f:(fun (a, _) -> f a) t

let partition_tf ~f t =
  Map.partition_tf ~f:(fun (a, _) -> f a) t

let reduce ~f t =
  Map.kreduce ~f t

let reduce_hlt ~f t =
  Map.kreduce_hlt ~f t

let to_list t =
  fold ~init:[] ~f:(fun accum a -> a :: accum) t

module SetToArray = struct
  include Seq
  include Array.Seq.MakePoly2(Seq)
end

let to_array t =
  SetToArray.(to_array (init t))
