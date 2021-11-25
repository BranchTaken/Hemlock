module T = struct
  type ('a, 'cmp) t = ('a, unit, 'cmp) Ordmap.t
  type 'a elm = 'a

  type ('a, 'cmp) cmper = ('a, 'cmp) Ordmap.cmper

  let length = Ordmap.length

  let is_empty = Ordmap.is_empty

  let fold_until ~init ~f t =
    Ordmap.fold_until ~init ~f:(fun accum (k, _) -> f accum k) t

  let fold_right_until ~init ~f t =
    Ordmap.fold_right_until ~init ~f:(fun (k, _) accum -> f k accum) t
end
include T
include Container.MakePoly2Fold(T)
include Container.MakePoly2Array(T)

let hash_fold t state =
  Ordmap.hash_fold Unit.hash_fold t state

let cmper_m = Ordmap.cmper_m

let cmper = Ordmap.cmper

let empty = Ordmap.empty

let singleton m a =
  Ordmap.singleton m ~k:a ~v:()

let mem = Ordmap.mem

let choose t =
  match Ordmap.choose t with
  | Some (k, _) -> Some k
  | None -> None

let choose_hlt t =
  let k, _ = Ordmap.choose_hlt t in
  k

let nth_opt i t =
  match Ordmap.nth_opt i t with
  | Some (a, _) -> Some a
  | None -> None

let nth i t =
  let a, _ = Ordmap.nth i t in
  a

let psearch = Ordmap.psearch

let search = Ordmap.search

let nsearch = Ordmap.nsearch

(* Seq. *)
module SeqPoly2Fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type ('a, 'cmp) t = ('a, unit, 'cmp) Ordmap.Seq.t

  let init = Ordmap.Seq.init

  let length = Ordmap.Seq.length

  let next t =
    let (a, _), t' = Ordmap.Seq.next t in
    a, t'

  let next_opt t =
    match Ordmap.Seq.next_opt t with
    | Some ((a, _), t') -> Some (a, t')
    | None -> None

  let cmper = Ordmap.Seq.cmper

  let cmp = Ordmap.Seq.cmp
end
include Seq.MakePoly2Fold2(SeqPoly2Fold2)
module Seq = SeqPoly2Fold2

let cmp t0 t1 =
  Ordmap.cmp Unit.cmp t0 t1

let equal t0 t1 =
  Ordmap.equal Unit.( = ) t0 t1

let subset t0 t1 =
  Ordmap.subset Unit.( = ) t0 t1

let disjoint t0 t1 =
  Ordmap.disjoint t0 t1

let insert a t =
  Ordmap.insert ~k:a ~v:() t

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist ordset = begin
        match alist with
        | [] -> ordset
        | a :: alist' -> fn alist' (insert a ordset)
      end in
      fn alist' (singleton m a)
    end

let remove = Ordmap.remove

let split a t =
  match Ordmap.split a t with
  | l, Some (a, _), r -> l, Some a, r
  | l, None, r -> l, None, r

let union t0 t1 =
  Ordmap.union ~f:(fun _ _ _ -> ()) t0 t1

let of_array m arr =
  match arr with
  | [||] -> empty m
  | _ -> Array.reduce_hlt (Array.map arr ~f:(fun a -> singleton m a))
    ~f:(fun ordset0 ordset1 -> union ordset0 ordset1)

let inter t0 t1 =
  Ordmap.inter ~f:(fun _ _ _ -> ()) t0 t1

let diff = Ordmap.diff

let filter ~f t =
  Ordmap.filter ~f:(fun (a, _) -> f a) t

let filteri ~f t =
  Ordmap.filteri ~f:(fun i (a, _) -> f i a) t

let partition_tf ~f t =
  Ordmap.partition_tf ~f:(fun (a, _) -> f a) t

let partitioni_tf ~f t =
  Ordmap.partitioni_tf ~f:(fun i (a, _) -> f i a) t

let reduce ~f t =
  Ordmap.kreduce ~f t

let reduce_hlt ~f t =
  Ordmap.kreduce_hlt ~f t
