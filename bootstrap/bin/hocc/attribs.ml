open Basis
open! Basis.Rudiments

module T = struct
  type t = (Attrib.K.t, Attrib.t, Attrib.K.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold (fun attrib state -> state |> Attrib.hash_fold attrib)

  let cmp = Ordmap.cmp (fun attrib0 attrib1 -> Attrib.cmp attrib0 attrib1)

  let fmt ?(alt=false) ?(width=0L) t formatter =
    formatter
    |> Ordmap.fmt ~alt ~width (fun attrib formatter ->
      formatter |> Attrib.pp attrib
    ) t

  let pp = Ordmap.pp (fun attrib formatter -> formatter |> Attrib.pp attrib)

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    let attrib_lst =
      Ordmap.fold_right ~init:[] ~f:(fun attrib_lst (_, attrib) -> attrib :: attrib_lst) t in
    formatter
    |> (fun formatter ->
      List.fmt ~alt ~width (Attrib.fmt_hr symbols prods ~alt ~width) attrib_lst formatter
    )
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal (fun attrib0 attrib1 -> Attrib.equal attrib0 attrib1) t0 t1

module Seq = struct
  type container = t
  type elm = Attrib.K.t * Attrib.t
  type t = (Attrib.K.t, Attrib.t, Attrib.K.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Attrib.K)

let singleton attrib =
  Ordmap.singleton (module Attrib.K) ~k:Attrib.(attrib.k) ~v:attrib

let is_empty = Ordmap.is_empty

let get symbol_index t =
  let k = Attrib.K.init ~symbol_index ~conflict:Contrib.empty in
  Ordmap.get k t

let amend = Ordmap.amend

let insert (Attrib.{k; _} as attrib) t =
  amend k ~f:(function
    | None -> Some attrib
    | Some (Attrib.{k=k_prev; _} as attrib_prev) -> begin
        assert Contrib.(Attrib.K.(k.conflict) = Attrib.K.(k_prev.conflict));
        Some (Attrib.union attrib_prev attrib)
      end
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _k (Attrib.{k=k0; _} as attrib0) (Attrib.{k=k1; _} as attrib1) ->
    assert Contrib.(Attrib.K.(k0.conflict) = Attrib.K.(k1.conflict));
    Attrib.union attrib0 attrib1
  ) t0 t1

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_k, attrib) -> f accum attrib) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_k, attrib) -> f accum attrib) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (_k, attrib) -> f attrib) t

let fold2_until ~init ~f t =
  Ordmap.fold2_until ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    f accum kv_opt0 kv_opt1
  ) t

let fold2 ~init ~f t =
  Ordmap.fold2 ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    f accum kv_opt0 kv_opt1
  ) t

let symbol_indexes t =
  fold ~init:(Ordset.empty (module Symbol.Index))
    ~f:(fun symbol_indexes Attrib.{k=K.{symbol_index; _}; _} ->
      Ordset.insert symbol_index symbol_indexes
    ) t
