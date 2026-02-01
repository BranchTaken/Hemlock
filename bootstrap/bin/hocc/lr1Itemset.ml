open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    items: (Lr0Item.t, Lr1Item.t, Lr0Item.cmper_witness) Ordmap.t;
    core: Lr0Itemset.t;
  }

  let hash_fold {items; _} state =
    state |> Ordmap.hash_fold Lr1Item.hash_fold items

  let cmp {items=i0; _} {items=i1; _} =
    Ordmap.cmp Lr1Item.cmp i0 i1

  let pp {items; core} formatter =
    formatter
    |> Fmt.fmt "{items=" |> Ordmap.pp Lr1Item.pp items
    |> Fmt.fmt "; core=" |> Lr0Itemset.pp core
    |> Fmt.fmt "}"

  let fmt_hr symbols ?(alt=false) ?(width=0L) {items; _} formatter =
    List.fmt ~alt ~width (fun (_lr0item, lr1item) formatter ->
      formatter
      |> Lr1Item.pp_hr symbols lr1item
    ) (Ordmap.to_alist items) formatter
end
include T
include Identifiable.Make(T)

module Seq = struct
  type container = t
  type elm = Lr1Item.t
  type t = (Lr0Item.t, Lr1Item.t, Lr0Item.cmper_witness) Ordmap.Seq.t

  let length = Ordmap.Seq.length
  let next t =
    match Ordmap.Seq.next t with (_, lr1item), t' -> lr1item, t'

  let next_opt t =
    match Ordmap.Seq.next_opt t with
    | None -> None
    | Some ((_, lr1item), t') -> Some (lr1item, t')

  let init {items; _} =
    Ordmap.Seq.init items
end

let empty =
  {
    items=Ordmap.empty (module Lr0Item);
    core=Lr0Itemset.empty;
  }

let singleton (Lr1Item.{lr0item; _} as lr1item) =
  {
    items=Ordmap.singleton (module Lr0Item) ~k:lr0item ~v:lr1item;
    core=Lr0Itemset.singleton lr0item;
  }

let length {items; _} =
  Ordmap.length items

let is_empty {items; _} =
  Ordmap.is_empty items

let choose {items; _} =
  match Ordmap.choose items with
  | None -> None
  | Some (_, lr1item) -> Some lr1item

let get Lr1Item.{lr0item; follow} {items; _} =
  match Ordmap.mem lr0item items with
  | false -> None
  | true -> begin
      let Lr1Item.{follow=t_follow; _} = Ordmap.get_hlt lr0item items in
      match Bitset.subset t_follow follow with
      | false -> None
      | true -> Ordmap.get lr0item items
    end

let mem lr1item t =
  Option.is_some (get lr1item t)

let insert (Lr1Item.{lr0item; follow} as lr1item) ({items; core} as t) =
  match Ordmap.get lr0item items with
  | None -> {items=Ordmap.insert ~k:lr0item ~v:lr1item items; core=Lr0Itemset.insert lr0item core}
  | Some Lr1Item.{follow=t_follow; _} -> begin
      let lr1item' = Lr1Item.init ~lr0item ~follow:(Bitset.union follow t_follow) in
      {t with items=Ordmap.update_hlt ~k:lr0item ~v:lr1item' items}
    end

let insert_hlt (Lr1Item.{lr0item; follow} as lr1item) ({items; core} as t) =
  match Ordmap.get lr0item items with
  | None -> {items=Ordmap.insert ~k:lr0item ~v:lr1item items; core=Lr0Itemset.insert lr0item core}
  | Some Lr1Item.{follow=t_follow; _} -> begin
      let t_follow' = Bitset.union follow t_follow in
      match Cmp.is_eq (Bitset.cmp t_follow t_follow') with
      | true -> halt "Item already present"
      | false -> begin
          let lr1item' = Lr1Item.init ~lr0item ~follow:t_follow' in
          {t with items=Ordmap.update_hlt ~k:lr0item ~v:lr1item' items}
        end
    end

let remove Lr1Item.{lr0item; follow} ({items; core} as t) =
  match Ordmap.get lr0item items with
  | None -> t
  | Some Lr1Item.{follow=t_follow; _} -> begin
      let follow' = Bitset.diff t_follow follow in
      match Bitset.is_empty follow' with
      | true -> {items=Ordmap.remove lr0item items; core=Lr0Itemset.remove lr0item core}
      | false -> begin
          let lr1item' = Lr1Item.init ~lr0item ~follow:follow' in
          {t with items=Ordmap.update_hlt ~k:lr0item ~v:lr1item' items}
        end
    end

let fold_until ~init ~f {items; _} =
  Ordmap.fold_until ~init ~f:(fun accum (_, lr1item) -> f accum lr1item) items

let fold ~init ~f {items; _} =
  Ordmap.fold ~init ~f:(fun accum (_, lr1item) -> f accum lr1item) items

let union t0 t1 =
  fold ~init:t1 ~f:(fun t lr1item -> insert lr1item t) t0

let inter t0 t1 =
  Ordmap.fold2 ~init:empty ~f:(fun t lr1item_opt0 lr1item_opt1 ->
    match lr1item_opt0, lr1item_opt1 with
    | Some _, None
    | None, Some _ -> t
    | Some (_, lr1item0), Some (_, lr1item1) -> begin
        let follow = Bitset.inter Lr1Item.(lr1item0.follow) Lr1Item.(lr1item1.follow) in
        match Bitset.is_empty follow with
        | true -> t
        | false -> begin
            let lr1item = Lr1Item.init ~lr0item:Lr1Item.(lr1item0.lr0item) ~follow in
            insert lr1item t
          end
      end
    | None, None -> not_reached ()
  ) t0.items t1.items

let diff t0 t1 =
  Ordmap.fold2 ~init:empty ~f:(fun t lr1item_opt0 lr1item_opt1 ->
    match lr1item_opt0, lr1item_opt1 with
    | Some (_, lr1item0), None -> insert lr1item0 t
    | None, Some _ -> t
    | Some (_, lr1item0), Some (_, lr1item1) -> begin
        let follow = Bitset.diff Lr1Item.(lr1item0.follow) Lr1Item.(lr1item1.follow) in
        match Bitset.is_empty follow with
        | true -> t
        | false -> begin
            let lr1item = Lr1Item.init ~lr0item:Lr1Item.(lr1item0.lr0item) ~follow in
            insert lr1item t
          end
      end
    | None, None -> not_reached ()
  ) t0.items t1.items

let core {core; _} =
  core

let is_start {items; _} =
  Ordmap.fold_until ~init:false ~f:(fun _ (Lr0Item.{dot; _}, _lr1item) ->
    let is_start = Uns.(dot = 0L) in
    is_start, is_start
  ) items

let start_symbol_index {items; _} =
  Ordmap.fold_until ~init:None ~f:(fun _ (Lr0Item.{prod={lhs_index; _}; dot}, _lr1item) ->
    let is_start = Uns.(dot = 0L) in
    match is_start with
    | false -> None, false
    | true -> Some lhs_index, true
  ) items
  |> Option.value_hlt

let is_accept t =
  fold_until ~init:true ~f:(fun _ lr1item ->
    match Lr1Item.is_accept lr1item with is_accept -> is_accept, not is_accept
  ) t

let compat_lr1 ({core=c0; _} as t0) ({core=c1; _} as t1) =
  assert Lr0Itemset.(c0 = c1);
  t0 = t1

let compat_pgm1 ({core=c0; _} as t0) ({core=c1; _} as t1) =
  let rec f o_seq t_seq = begin
    let rec compat_weak_follow_inner o_seq t_seq o_follow t_follow = begin
      match Seq.next_opt o_seq, Seq.next_opt t_seq with
      | None, None -> true
      | Some (Lr1Item.{follow=o_follow'; _}, o_seq'), Some (Lr1Item.{follow=t_follow'; _}, t_seq')
        -> begin
            (* Require weakly compatible follow sets for all follow set pairings, as defined by the
             * Pager(1977) algorithm, and as refined by Menhir to prevent phantom conflicts
             * accompanying actual conflicts. *)
            match
              (Bitset.subset (Bitset.union t_follow o_follow') (Bitset.inter o_follow t_follow')),
              (Bitset.subset (Bitset.union o_follow t_follow') (Bitset.inter t_follow o_follow'))
            with
            | true, true -> compat_weak_follow_inner o_seq' t_seq' o_follow t_follow
            | _ -> false
          end
      | None, Some _
      | Some _, None -> not_reached ()
    end in
    match Seq.next_opt o_seq, Seq.next_opt t_seq with
    | None, None -> true
    | Some (Lr1Item.{follow=o_follow; _}, o_seq'), Some (Lr1Item.{follow=t_follow; _}, t_seq') ->
      compat_weak_follow_inner o_seq' t_seq' o_follow t_follow && f o_seq' t_seq'
    | None, Some _
    | Some _, None -> not_reached ()
  end in
  assert Lr0Itemset.(c0 = c1);
  match Uns.(=) (length t0) (length t1) with
  | false -> false
  | true -> begin
      let o_seq = Seq.init t0 in
      let t_seq = Seq.init t1 in
      f o_seq t_seq
    end

let compat_lalr1 {core=c0; _} {core=c1; _} =
  assert Lr0Itemset.(c0 = c1);
  true
