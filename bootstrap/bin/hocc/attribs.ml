open Basis
open! Basis.Rudiments

module T = struct
  type t = (Attrib.K.t, Attrib.t, Attrib.K.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold (fun Attrib.{k=_; v} state -> state |> Attrib.V.hash_fold v)

  let cmp = Ordmap.cmp (fun Attrib.{k=_; v=v0} {k=_; v=v1} -> Attrib.V.cmp v0 v1)

  let fmt ?(alt=false) ?(width=0L) t formatter =
    formatter
    |> Ordmap.fmt ~alt ~width (fun Attrib.{k=_; v} formatter ->
      formatter |> Attrib.V.pp v
    ) t

  let pp = Ordmap.pp (fun Attrib.{k=_; v} formatter -> formatter |> Attrib.V.pp v)

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
  let open Attrib in
  Ordmap.equal (fun {k=_; v=v0} {k=_; v=v1} -> V.equal v0 v1) t0 t1

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
  let akey = Attrib.K.init ~symbol_index ~conflict:Contrib.empty in
  Ordmap.get akey t

let amend akey ~f t =
  Ordmap.amend akey ~f:(fun attrib_option ->
    let aval_opt = match attrib_option with
      | None -> f None
      | Some Attrib.{k=_; v} -> f (Some v)
    in
    match aval_opt with
    | None -> None
    | Some aval -> Some (Attrib.init akey aval)
  ) t

let insert (Attrib.{k; v=_} as attrib) t =
  Ordmap.amend k ~f:(function
    | None -> Some attrib
    | Some (Attrib.{k=akey_prev; v=_} as attrib_prev) -> begin
        assert Contrib.(Attrib.K.(k.conflict) = Attrib.K.(akey_prev.conflict));
        Some (Attrib.union attrib_prev attrib)
      end
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _k (Attrib.{k=k0; v=_} as attrib0) (Attrib.{k=k1; v=_} as attrib1) ->
    assert Contrib.(Attrib.K.(k0.conflict) = Attrib.K.(k1.conflict));
    Attrib.union attrib0 attrib1
  ) t0 t1

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_akey, akey_aval) -> f accum akey_aval) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_akey, akey_aval) -> f accum akey_aval) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (_akey, akey_aval) -> f akey_aval) t

let fold2_until ~init ~f t =
  Ordmap.fold2_until ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_akey, attrib) -> Some attrib
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_akey, attrib) -> Some attrib
    in
    f accum kv_opt0 kv_opt1
  ) t

let fold2 ~init ~f t =
  Ordmap.fold2 ~init ~f:(fun accum k_kv_opt0 k_kv_opt1 ->
    let kv_opt0 = match k_kv_opt0 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    f accum kv_opt0 kv_opt1
  ) t

let symbol_indexes t =
  fold ~init:(Ordset.empty (module Symbol.Index))
    ~f:(fun symbol_indexes Attrib.{k=K.{symbol_index; _}; v=_} ->
      Ordset.insert symbol_index symbol_indexes
    ) t
