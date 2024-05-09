open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    symbol_attribs: (Symbol.Index.t, Attrib.t, Symbol.Index.cmper_witness) Ordmap.t
  }

  let hash_fold {symbol_attribs; _} =
    Ordmap.hash_fold (fun attrib state -> state |> Attrib.hash_fold attrib) symbol_attribs

  let cmp {symbol_attribs=sa0; _} {symbol_attribs=sa1; _} =
    Ordmap.cmp (fun attrib0 attrib1 -> Attrib.cmp attrib0 attrib1) sa0 sa1

  let fmt ?(alt=false) ?(width=0L) {symbol_attribs} formatter =
    formatter
    |> Fmt.fmt "{symbol_attribs="
    |> Ordmap.fmt ~alt ~width (fun attrib formatter ->
      formatter |> Attrib.pp attrib
    ) symbol_attribs
    |> Fmt.fmt "}"

  let pp {symbol_attribs} formatter =
    formatter
    |> Fmt.fmt "{symbol_attribs="
    |> Ordmap.pp (fun attrib formatter -> formatter |> Attrib.pp attrib) symbol_attribs
    |> Fmt.fmt "}"

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) {symbol_attribs}
    formatter =
    let attrib_lst = Ordmap.fold_right ~init:[]
      ~f:(fun attrib_lst (_, attrib) -> attrib :: attrib_lst) symbol_attribs in
    formatter
    |> Fmt.fmt "{symbol_attribs="
    |> (fun formatter ->
      List.fmt ~alt ~width (Attrib.fmt_hr symbols prods ~alt ~width) attrib_lst formatter
    )
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let length {symbol_attribs} =
  Ordmap.length symbol_attribs

let equal {symbol_attribs=sa0} {symbol_attribs=sa1} =
  Ordmap.equal (fun attrib0 attrib1 -> Attrib.equal attrib0 attrib1) sa0 sa1

module Seq = struct
  type container = t
  type elm = Attrib.t
  type t = (Symbol.Index.t, Attrib.t, Symbol.Index.cmper_witness) Ordmap.Seq.t

  let init {symbol_attribs} =
    Ordmap.Seq.init symbol_attribs
  let length = Ordmap.Seq.length
  let next seq =
    match Ordmap.Seq.next seq with
    | (_symbol_index, attrib), seq' -> attrib, seq'
  let next_opt seq =
    match Ordmap.Seq.next_opt seq with
    | None -> None
    | Some ((_symbol_index, attrib), seq') -> Some (attrib, seq')
end

let empty =
  {symbol_attribs=Ordmap.empty (module Symbol.Index)}

let singleton attrib =
  {
    symbol_attribs=Ordmap.singleton (module Symbol.Index) ~k:Attrib.(attrib.symbol_index) ~v:attrib
  }

let is_empty {symbol_attribs} =
  Ordmap.is_empty symbol_attribs

let get symbol_index {symbol_attribs} =
  Ordmap.get symbol_index symbol_attribs

let amend symbol_index ~f {symbol_attribs} =
  let symbol_attribs' = Ordmap.amend symbol_index ~f:(fun attrib_opt ->
    let attrib_opt' = f attrib_opt in
    let () = match attrib_opt, attrib_opt' with
      | Some attrib, Some attrib' -> assert (Attrib.equal_keys attrib attrib');
      | Some _, None
      | None, Some _
      | None, None -> ()
    in
    attrib_opt'
  ) symbol_attribs in
  {symbol_attribs=symbol_attribs'}

let insert (Attrib.{symbol_index; _} as attrib) t =
  amend symbol_index ~f:(function
    | None -> Some attrib
    | Some attrib_prev -> begin
        assert (Attrib.equal_keys attrib attrib_prev);
        Some (Attrib.union attrib_prev attrib)
      end
  ) t

let union {symbol_attribs=sa0} {symbol_attribs=sa1} =
  let symbol_attribs = Ordmap.union ~f:(fun _k attrib0 attrib1 ->
    assert (Attrib.equal_keys attrib0 attrib1);
    Attrib.union attrib0 attrib1
  ) sa0 sa1 in
  {symbol_attribs}

let reindex index_map ({symbol_attribs} as t) =
  assert (not (is_empty t));
  Ordmap.fold ~init:empty
    ~f:(fun reindexed_t (_symbol_index, (Attrib.{conflict_state_index; _} as attrib)) ->
      match Map.get conflict_state_index index_map with
      | None -> reindexed_t
      | Some conflict_state_index' ->
        insert {attrib with conflict_state_index=conflict_state_index'} reindexed_t
    ) symbol_attribs

let fold_until ~init ~f {symbol_attribs} =
  Ordmap.fold_until ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) symbol_attribs

let fold ~init ~f {symbol_attribs} =
  Ordmap.fold ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) symbol_attribs

let fold2_until ~init ~f {symbol_attribs=sa0} {symbol_attribs=sa1} =
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
  ) sa0 sa1

let fold2 ~init ~f {symbol_attribs=sa0} {symbol_attribs=sa1} =
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
  ) sa0 sa1
