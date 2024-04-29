open Basis
open! Basis.Rudiments

module T = struct
  type t = (Attrib.K.t, Attrib.K.t * Attrib.V.t, Attrib.K.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold (fun (_akey, aval) state -> state |> Attrib.V.hash_fold aval)

  let cmp = Ordmap.cmp (fun (_akey0, aval0) (_akey1, aval1) -> Attrib.V.cmp aval0 aval1)

  let fmt ?(alt=false) ?(width=0L) t formatter =
    formatter |> Ordmap.fmt ~alt ~width (fun (_akey, aval) formatter -> formatter |> Attrib.V.pp aval) t

  let pp = Ordmap.pp (fun (_akey, aval) formatter -> formatter |> Attrib.V.pp aval)

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    formatter
    |> (fun formatter ->
      List.fmt ~alt ~width (fun (symbol, aval) formatter ->
        formatter
        |> Attrib.K.pp_hr symbols prods symbol
        |> Fmt.fmt " = "
        |> (fun (_akey, aval) formatter ->
          formatter |> Attrib.V.fmt_hr symbols prods ~alt ~width:(width + 4L) aval
        ) aval
      ) (Ordmap.to_alist t) formatter
    )
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal (fun (_akey0, aval0) (_akey1, aval1) -> Attrib.V.equal aval0 aval1) t0 t1

module Seq = struct
  type container = t
  type elm = Attrib.K.t * (Attrib.K.t * Attrib.V.t)
  type t = (Attrib.K.t, Attrib.K.t * Attrib.V.t, Attrib.K.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Attrib.K)

let singleton akey aval =
  Ordmap.singleton (module Attrib.K) ~k:akey ~v:(akey, aval)

let is_empty = Ordmap.is_empty

let get symbol_index t =
  let akey = Attrib.K.init ~symbol_index ~conflict:Contrib.empty in
  Ordmap.get akey t

let amend akey ~f t =
  Ordmap.amend akey ~f:(fun akey_aval_option ->
    let aval_opt = match akey_aval_option with
      | None -> f None
      | Some (_akey, aval) -> f (Some aval)
    in
    match aval_opt with
    | None -> None
    | Some aval -> Some (akey, aval)
  ) t

let insert akey aval t =
  Ordmap.amend akey ~f:(function
    | None -> Some (akey, aval)
    | Some (akey_prev, aval_prev) -> begin
        assert Contrib.(Attrib.K.(akey.conflict) = Attrib.K.(akey_prev.conflict));
        Some (akey_prev, Attrib.V.union aval aval_prev)
      end
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _akey (akey0, aval0) (akey1, aval1) ->
    assert Contrib.(Attrib.K.(akey0.conflict) = Attrib.K.(akey1.conflict));
    akey0, Attrib.V.union aval0 aval1
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
      | Some (_akey, akey_aval) -> Some akey_aval
    in
    let kv_opt1 = match k_kv_opt1 with
      | None -> None
      | Some (_akey, akey_aval) -> Some akey_aval
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
    ~f:(fun symbol_indexes (Attrib.K.{symbol_index; _}, _aval) ->
      Ordset.insert symbol_index symbol_indexes
    ) t
