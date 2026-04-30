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

  let init ~conflict_state_index ~symbol_index =
    {conflict_state_index; symbol_index}
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

  let fmt_hr precs symbols prods ?(alt=false) ?(width=0L) t
    formatter =
    let attrib_lst = Ordmap.fold_right ~init:[]
      ~f:(fun attrib_lst (_, attrib) -> attrib :: attrib_lst) t in
    formatter
    |> (fun formatter ->
      List.fmt ~alt ~width (Attrib.fmt_hr precs symbols prods ~alt ~width:(width+4L)) attrib_lst
        formatter
    )
end
include T
include Identifiable.Make(T)

let length =
  Ordmap.length

let equal t0 t1 =
  Ordmap.equal ~vequal:(fun _k attrib0 attrib1 -> Attrib.equal attrib0 attrib1) t0 t1

module Seq = struct
  type container = t
  type elm = Attrib.t
  type t = (K.t, Attrib.t, K.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next seq =
    match Ordmap.Seq.next seq with
    | (_k, attrib), seq' -> attrib, seq'
  let next_opt seq =
    match Ordmap.Seq.next_opt seq with
    | None -> None
    | Some ((_k, attrib), seq') -> Some (attrib, seq')
end

let empty = Ordmap.empty (module K)

let singleton (Attrib.{conflict_state_index; symbol_index; _} as attrib) =
  let k = K.init ~conflict_state_index ~symbol_index in
  Ordmap.singleton (module K) ~k ~v:attrib

let is_empty = Ordmap.is_empty

let get ~conflict_state_index ~symbol_index t =
  let k = K.init ~conflict_state_index ~symbol_index in
  Ordmap.get k t

let get_hlt ~conflict_state_index ~symbol_index t =
  let k = K.init ~conflict_state_index ~symbol_index in
  Ordmap.get_hlt k t

let insert (Attrib.{conflict_state_index; symbol_index; _} as attrib) t =
  assert (not (Attrib.is_empty attrib));
  let k = K.init ~conflict_state_index ~symbol_index in
  match Ordmap.get k t with
  | None -> Ordmap.insert_hlt ~k ~v:attrib t
  | Some attrib_prev -> begin
      let attrib = Attrib.union attrib_prev attrib in
      Ordmap.update_hlt ~k ~v:attrib t
    end

let union t0 t1 =
  Ordmap.union ~vunion:(fun _k attrib0 attrib1 ->
    Attrib.union attrib0 attrib1
  ) t0 t1

(* Not used. *)
let inter t0 t1 =
  Ordmap.inter ~vinter:(fun _k attrib0 attrib1 ->
    let attrib = Attrib.inter attrib0 attrib1 in
    match Attrib.is_empty attrib with
    | true -> None
    | false -> Some attrib
  ) t0 t1

(* Not used. *)
let diff t0 t1 =
  Ordmap.diff ~vdiff:(fun _k attrib0 attrib1 ->
    let attrib = Attrib.diff attrib0 attrib1 in
    match Attrib.is_empty attrib with
    | true -> None
    | false -> Some attrib
  ) t0 t1

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (_k, attrib) -> f accum attrib) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (_k, attrib) -> f accum attrib) t

let for_any ~f t =
  Ordmap.for_any ~f:(fun (_k, attrib) -> f attrib) t

let fold2_until ~init ~f t0 t1 =
  Ordmap.fold2_until ~init ~f:(fun accum kv_opt0 kv_opt1 ->
    let v_opt0 = match kv_opt0 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    let v_opt1 = match kv_opt1 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    f accum v_opt0 v_opt1
  ) t0 t1

let fold2 ~init ~f t0 t1 =
  Ordmap.fold2 ~init ~f:(fun accum kv_opt0 kv_opt1 ->
    let v_opt0 = match kv_opt0 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    let v_opt1 = match kv_opt1 with
      | None -> None
      | Some (_k, attrib) -> Some attrib
    in
    f accum v_opt0 v_opt1
  ) t0 t1
