open Basis
open! Basis.Rudiments

module K = struct
  module T = struct
    type t = {
      conflict_state_index: StateIndex.t;
      symbol_index: Symbol.Index.t;
    }

    let hash_fold {conflict_state_index; symbol_index} state =
      state
      |> Uns.hash_fold 1L |> StateIndex.hash_fold conflict_state_index
      |> Uns.hash_fold 2L |> Symbol.Index.hash_fold symbol_index

    let cmp {conflict_state_index=csi0; symbol_index=s0}
      {conflict_state_index=csi1; symbol_index=s1} =
      let open Cmp in
      match StateIndex.cmp csi0 csi1 with
      | Lt -> Lt
      | Eq -> Symbol.Index.cmp s0 s1
      | Gt -> Gt

    let pp {conflict_state_index; symbol_index} formatter =
      formatter
      |> Fmt.fmt "{conflict_state_index=" |> StateIndex.pp conflict_state_index
      |> Fmt.fmt "; symbol_index=" |> Symbol.Index.pp symbol_index
      |> Fmt.fmt "}"
  end
  include T
  include Identifiable.Make(T)
end

module T = struct
  type t = (K.t, Attrib.t, K.cmper_witness) Ordmap.t

  let hash_fold t =
    Ordmap.hash_fold (fun attrib state -> state |> Attrib.hash_fold attrib) t

  let cmp t0 t1 =
    Ordmap.cmp (fun attrib0 attrib1 -> Attrib.cmp attrib0 attrib1) t0 t1

  let fmt ?(alt=false) ?(width=0L) t formatter =
    formatter
    |> Ordmap.fmt ~alt ~width (fun attrib formatter ->
      formatter |> Attrib.pp attrib
    ) t

  let pp t formatter =
    formatter
    |> Ordmap.pp (fun attrib formatter -> formatter |> Attrib.pp attrib) t

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t
    formatter =
    let attrib_lst = Ordmap.fold_right ~init:[]
      ~f:(fun attrib_lst (_, attrib) -> attrib :: attrib_lst) t in
    formatter
    |> (fun formatter ->
      List.fmt ~alt ~width (Attrib.fmt_hr symbols prods ~alt ~width:(width+4L)) attrib_lst formatter
    )
end
include T
include Identifiable.Make(T)

let length =
  Ordmap.length

let equal t0 t1 =
  Ordmap.equal (fun attrib0 attrib1 -> Attrib.equal attrib0 attrib1) t0 t1

module Seq = struct
  type container = t
  type elm = Attrib.t
  type t = (K.t, Attrib.t, K.cmper_witness) Ordmap.Seq.t

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

let empty = Ordmap.empty (module K)

let singleton (Attrib.{conflict_state_index; symbol_index; _} as attrib) =
  let k = K.{conflict_state_index; symbol_index} in
  Ordmap.singleton (module K) ~k ~v:attrib

let is_empty = Ordmap.is_empty

let get ~conflict_state_index ~symbol_index t =
  let k = K.{conflict_state_index; symbol_index} in
  Ordmap.get k t

let get_hlt ~conflict_state_index ~symbol_index t =
  let k = K.{conflict_state_index; symbol_index} in
  Ordmap.get_hlt k t

let amend_impl attrib_equalish_keys ~conflict_state_index ~symbol_index ~f t =
  let k = K.{conflict_state_index; symbol_index} in
  Ordmap.amend k ~f:(fun attrib_opt ->
    let attrib_opt' = f attrib_opt in
    let () = match attrib_opt, attrib_opt' with
      | Some attrib, Some attrib' -> assert (attrib_equalish_keys attrib attrib');
      | Some _, None
      | None, Some _
      | None, None -> ()
    in
    attrib_opt'
  ) t

let amend ~conflict_state_index ~symbol_index ~f t =
  amend_impl Attrib.equal_keys ~conflict_state_index ~symbol_index ~f t

let insert_impl attrib_equalish_keys (Attrib.{conflict_state_index; symbol_index; _} as attrib) t =
  assert (not (Attrib.is_empty attrib));
  amend_impl attrib_equalish_keys ~conflict_state_index ~symbol_index ~f:(function
    | None -> Some attrib
    | Some attrib_prev -> begin
        assert (attrib_equalish_keys attrib attrib_prev);
        Some (Attrib.union_remerged attrib_prev attrib)
      end
  ) t

let insert attrib t =
  insert_impl Attrib.equal_keys attrib t

let insert_remerged attrib t =
  insert_impl Attrib.remergeable_keys attrib t

let union t0 t1 =
  Ordmap.union ~f:(fun _k attrib0 attrib1 ->
    assert (Attrib.equal_keys attrib0 attrib1);
    Attrib.union attrib0 attrib1
  ) t0 t1

(* Not used. *)
let inter t0 t1 =
  match is_empty t0, is_empty t1 with
  | true, _
  | _, true -> empty
  | false, false -> begin
      Ordmap.fold2 ~init:empty ~f:(fun t k_attrib0_opt k_attrib1_opt ->
        match k_attrib0_opt, k_attrib1_opt with
        | Some _, None
        | None, Some _ -> t
        | Some (k, attrib0), Some (_k, attrib1) -> begin
            let attrib = Attrib.inter attrib0 attrib1 in
            match Attrib.is_empty attrib with
            | true -> t
            | false -> Ordmap.insert ~k ~v:attrib t
          end
        | None, None -> not_reached ()
      ) t0 t1
    end

(* Not used. *)
let diff t0 t1 =
  match is_empty t0, is_empty t1 with
  | true, _ -> empty
  | _, true -> t0
  | false, false -> begin
      Ordmap.fold2 ~init:empty ~f:(fun t k_attrib0_opt k_attrib1_opt ->
        match k_attrib0_opt, k_attrib1_opt with
        | Some (k, attrib), None -> Ordmap.insert ~k ~v:attrib t
        | None, Some _ -> t
        | Some (k, attrib0), Some (_k, attrib1) -> begin
            let attrib = Attrib.diff attrib0 attrib1 in
            match Attrib.is_empty attrib with
            | true -> t
            | false -> Ordmap.insert ~k ~v:attrib t
          end
        | None, None -> not_reached ()
      ) t0 t1
    end

let remerge1 remergeable_index_map t =
  Ordmap.fold ~init:empty
    ~f:(fun remerged_t (_symbol_index, attrib) ->
      insert_remerged (Attrib.remerge1 remergeable_index_map attrib) remerged_t
    ) t

let remerge remergeable_index_map t0 t1 =
  remerge1 remergeable_index_map (union t0 t1)

let reindex index_map t =
  Ordmap.fold ~init:empty
    ~f:(fun reindexed_t (_symbol_index, attrib) ->
      match Attrib.reindex index_map attrib with
      | None -> reindexed_t
      | Some attrib' -> insert_remerged attrib' reindexed_t
    ) t

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_symbol_index, attrib) -> f accum attrib) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (_symbol_index, attrib) -> f attrib) t

let fold2_until ~init ~f t0 t1 =
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
  ) t0 t1

let fold2 ~init ~f t0 t1 =
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
  ) t0 t1
