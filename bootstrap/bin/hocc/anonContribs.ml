open Basis
open! Basis.Rudiments

module Attribs = struct
  module T = struct
    type t = (Symbol.Index.t, Contrib.t, Symbol.Index.cmper_witness) Ordmap.t

    let hash_fold = Ordmap.hash_fold Contrib.hash_fold

    let cmp = Ordmap.cmp Contrib.cmp

    let pp = Ordmap.pp Contrib.pp

    let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
      formatter
      |> (fun formatter ->
        List.fmt ~alt ~width (fun (symbol_index, contrib) formatter ->
          formatter
          |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
          |> Fmt.fmt " = "
          |> Contrib.pp_hr symbols prods contrib
        ) (Ordmap.to_alist t) formatter
      )
  end
  include T
  include Identifiable.Make(T)

  let length = Ordmap.length

  let equal t0 t1 =
    Ordmap.equal Contrib.(=) t0 t1

  module Seq = struct
    type t = (Symbol.Index.t, Contrib.t, Symbol.Index.cmper_witness) Ordmap.Seq.t

    let init = Ordmap.Seq.init
    let length = Ordmap.Seq.length
    let next = Ordmap.Seq.next
  end

  let empty = Ordmap.empty (module Symbol.Index)

  let singleton symbol_index contrib =
    Ordmap.singleton (module Symbol.Index) ~k:symbol_index ~v:contrib

  let is_empty = Ordmap.is_empty

  let get = Ordmap.get

  let amend = Ordmap.amend

  let union t0 t1 =
    Ordmap.union ~f:(fun _symbol_index contrib0 contrib1 ->
      Contrib.union contrib0 contrib1
    ) t0 t1

  let fold_until = Ordmap.fold_until

  let fold = Ordmap.fold
end

module T = struct
  type t = (
    StateIndex.t, (* Conflict state. *)
    Attribs.t, (* Symbol -> contrib attributions. *)
    StateIndex.cmper_witness
  ) Ordmap.t

  let hash_fold t state =
    state |> Ordmap.hash_fold Attribs.hash_fold t

  let cmp t0 t1 =
    Ordmap.cmp Attribs.cmp t0 t1

  let fmt ?(alt=false) ?(width=0L) t formatter =
    match alt with
    | false -> formatter |> Ordmap.pp Attribs.pp t
    | true -> formatter |> Ordmap.fmt ~alt ~width Attribs.pp t

  let pp t formatter =
    fmt t formatter

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    List.fmt ~alt ~width (fun (conflict_state_index, attribs) formatter ->
      formatter
      |> StateIndex.pp conflict_state_index
      |> Fmt.fmt " = "
      |> Attribs.fmt_hr ~alt ~width:(width + 4L) symbols prods attribs
    ) (Ordmap.to_alist t) formatter
end
include T
include Identifiable.Make(T)

let length t =
  match Ordmap.is_empty t with
  | true -> 0L
  | false ->
    t
    |> Ordmap.map ~f:(fun (_conflict_state_index, attribs) -> Attribs.length attribs)
    |> Ordmap.reduce_hlt ~f:Uns.(+)

let equal t0 t1 =
  Ordmap.equal Attribs.equal t0 t1

module Seq = struct
  type container = t
  type elm = StateIndex.t * Symbol.Index.t * Contrib.t
  type t = {
    l: uns;
    conflict_state_index_opt: uns option;
    seq_inner_opt: Attribs.Seq.t option;
    seq_outer: (
      StateIndex.t,
      Attribs.t,
      StateIndex.cmper_witness
    ) Ordmap.Seq.t;
  }

  let init container =
    {
      l=length container;
      conflict_state_index_opt=None;
      seq_inner_opt=None;
      seq_outer=Ordmap.Seq.init container;
    }

  let length {l; _} =
    l

  let next t =
    let rec normalize ({l; seq_inner_opt; _} as t) = begin
      let advance ({seq_outer; _} as t) = begin
        let (conflict_state_index, symbol_contrib_map), seq_outer =
          Ordmap.Seq.next seq_outer in
        let seq_inner = Attribs.Seq.init symbol_contrib_map in
        normalize {t with
          conflict_state_index_opt=Some conflict_state_index;
          seq_inner_opt=Some seq_inner;
          seq_outer;
        }
      end in
      assert Uns.(l > 0L);
      match seq_inner_opt with
      | Some seq_inner -> begin
          match Attribs.Seq.length seq_inner with
          | 0L -> advance t
          | _ ->
            t.l,
            Option.value_hlt t.conflict_state_index_opt,
            Option.value_hlt t.seq_inner_opt,
            t.seq_outer
        end
      | None -> advance t
    end in
    match normalize t with
    | l, conflict_state_index, seq_inner, seq_outer -> begin
        let (symbol_index, contrib), seq_inner = Attribs.Seq.next seq_inner in
        (conflict_state_index, symbol_index, contrib), {
          l=pred l;
          conflict_state_index_opt=Some conflict_state_index;
          seq_inner_opt=Some seq_inner;
          seq_outer;
        }
      end

  let next_opt t =
    match length t with
    | 0L -> None
    | _ -> Some (next t)
end

let empty = Ordmap.empty (module StateIndex)

let singleton ~conflict_state_index symbol_index contrib =
  let attribs =  Attribs.singleton symbol_index contrib in
  match Contrib.is_empty contrib with
  | true -> empty
  | false ->  Ordmap.singleton (module StateIndex) ~k:conflict_state_index ~v:attribs

let reindex index_map t =
  Ordmap.fold ~init:empty ~f:(fun reindexed_t (state_index, attribs) ->
    match Map.get state_index index_map with
    | None -> reindexed_t
    | Some state_index' -> Ordmap.insert ~k:state_index' ~v:attribs reindexed_t
  ) t

let is_empty t =
  Uns.(=) (length t) 0L

let get ~conflict_state_index symbol_index t =
  match Ordmap.get conflict_state_index t with
  | None -> None
  | Some attribs -> begin
      match Attribs.get symbol_index attribs with
      | None -> None
      | Some contrib -> Some contrib
    end

let get_hlt ~conflict_state_index symbol_index t =
  get ~conflict_state_index symbol_index t
  |> Option.value_hlt

let contains ~conflict_state_index symbol_index contrib t =
  assert (not (Contrib.is_empty contrib));
  match get ~conflict_state_index symbol_index t with
  | None -> false
  | Some contrib_existing -> Contrib.(inter contrib_existing contrib = contrib)

let amend ~conflict_state_index symbol_index ~f t =
  let attribs = match Ordmap.get conflict_state_index t with
    | None -> Attribs.empty
    | Some attribs -> attribs
  in
  let attribs' = Attribs.amend symbol_index ~f attribs in
  Ordmap.upsert ~k:conflict_state_index ~v:attribs' t

let insert ~conflict_state_index symbol_index contrib t =
  match Contrib.is_empty contrib with
  | true -> t
  | false ->
    Ordmap.amend conflict_state_index t ~f:(function
      | None -> Some (Attribs.singleton symbol_index contrib)
      | Some attribs -> begin
          Some (
            Attribs.amend symbol_index attribs ~f:(function
              | None -> Some contrib
              | Some contrib_prev -> Some (Contrib.union contrib contrib_prev)
            )
          )
        end
    )

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (conflict_state_index, attribs) ->
    Attribs.fold_until ~init:(accum, false) ~f:(fun (accum, _) (symbol_index, contrib) ->
      let accum, until = f accum conflict_state_index symbol_index contrib in
      (accum, until), until
    ) attribs
  ) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (conflict_state_index, attribs) ->
    Attribs.fold ~init:accum ~f:(fun accum (symbol_index, contrib) ->
      f accum conflict_state_index symbol_index contrib
    ) attribs
  ) t

let union t0 t1 =
  Ordmap.fold2 ~init:empty ~f:(fun t state_symbol_contrib_opt0 state_symbol_contrib_opt1 ->
    let conflict_state_index, attribs =
      match state_symbol_contrib_opt0, state_symbol_contrib_opt1 with
      | Some (conflict_state_index, attribs), None
      | None, Some (conflict_state_index, attribs) -> conflict_state_index, attribs
      | Some (conflict_state_index, attribs0), Some (_, attribs1) ->
        conflict_state_index, Attribs.union attribs0 attribs1
      | None, None -> not_reached ()
    in
    match Attribs.is_empty attribs with
    | true -> t
    | false -> Ordmap.insert_hlt ~k:conflict_state_index ~v:attribs t
  ) t0 t1

let fold2_until ~init ~f t0 t1 =
  let rec inner ~f accum seq0 seq1 = begin
    let left state_index0 symbol_index0 contrib0 seq0' = begin
      let accum, until = f accum state_index0 symbol_index0 (Some contrib0) None in
      match until with
      | true -> accum
      | false -> inner ~f accum seq0' seq1
    end in
    let right state_index1 symbol_index1 contrib1 seq1' = begin
      let accum, until = f accum state_index1 symbol_index1 None (Some contrib1) in
      match until with
      | true -> accum
      | false -> inner ~f accum seq0 seq1'
    end in
    match Seq.next_opt seq0, Seq.next_opt seq1 with
    | None, None -> accum
    | Some ((state_index0, symbol_index0, contrib0), seq0'), None ->
      left state_index0 symbol_index0 contrib0 seq0'
    | None, Some ((state_index1, symbol_index1, contrib1), seq1') ->
      right state_index1 symbol_index1 contrib1 seq1'
    | Some ((state_index0, symbol_index0, contrib0), seq0'),
      Some ((state_index1, symbol_index1, contrib1), seq1') -> begin
        let rel = match Uns.cmp state_index0 state_index1 with
          | Cmp.Lt -> Cmp.Lt
          | Eq -> Uns.cmp symbol_index0 symbol_index1
          | Gt -> Gt
        in
        match rel with
        | Lt -> left state_index0 symbol_index0 contrib0 seq0'
        | Eq -> begin
            let accum, until = f accum state_index0 symbol_index0 (Some contrib0) (Some contrib1) in
            match until with
            | true -> accum
            | false -> inner ~f accum seq0' seq1'
          end
        | Gt -> right state_index1 symbol_index1 contrib1 seq1'
      end
  end in
  inner ~f init (Seq.init t0) (Seq.init t1)

let fold2 ~init ~f t0 t1 =
  fold2_until ~init ~f:(fun accum conflict_state_index symbol_index contrib_opt0 contrib_opt1 ->
    f accum conflict_state_index symbol_index contrib_opt0 contrib_opt1, false
  ) t0 t1
