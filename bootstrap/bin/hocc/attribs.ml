open Basis
open! Basis.Rudiments

module T = struct
  type t = (Symbol.Index.t, Attrib.t, Symbol.Index.cmper_witness) Ordmap.t

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
  type elm = Attrib.t
  type t = (Symbol.Index.t, Attrib.t, Symbol.Index.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next seq =
    match Ordmap.Seq.next seq with
    | (_symbol_index, attrib), seq' -> attrib, seq'
  let next_opt seq =
    match Ordmap.Seq.next_opt seq with
    | None -> None
    | Some ((_symbol_index, attrib), seq') -> Some (attrib, seq')
end

let empty = Ordmap.empty (module Symbol.Index)

let singleton attrib =
  Ordmap.singleton (module Symbol.Index) ~k:Attrib.(attrib.symbol_index) ~v:attrib

let is_empty = Ordmap.is_empty

let get symbol_index t =
  Ordmap.get symbol_index t

let amend symbol_index ~f =
  Ordmap.amend symbol_index ~f:(fun attrib_opt ->
    let attrib_opt' = f attrib_opt in
    let () = match attrib_opt, attrib_opt' with
      | Some Attrib.{conflict_state_index=csi0; symbol_index=x0; _},
        Some Attrib.{conflict_state_index=csi1; symbol_index=x1; _} -> begin
          assert StateIndex.(csi0 = csi1);
          assert Symbol.Index.(x0 = x1);
        end
      | Some _, None
      | None, Some _
      | None, None -> ()
    in
    attrib_opt'
  )

let insert (Attrib.{symbol_index; conflict; _} as attrib) t =
  amend symbol_index ~f:(function
    | None -> Some attrib
    | Some (Attrib.{conflict=conflict_prev; _} as attrib_prev) -> begin
        assert Contrib.(conflict = conflict_prev);
        Some (Attrib.union attrib_prev attrib)
      end
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _k (Attrib.{conflict=x0; _} as attrib0)
    (Attrib.{conflict=x1; _} as attrib1) ->
    assert Contrib.(x0 = x1);
    Attrib.union attrib0 attrib1
  ) t0 t1

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) t

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
    ~f:(fun symbol_indexes Attrib.{symbol_index; _} ->
      Ordset.insert symbol_index symbol_indexes
    ) t
