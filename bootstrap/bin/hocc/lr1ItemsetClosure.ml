open Basis
open Basis.Rudiments

module Action = struct
  module T = struct
    type t =
      | ShiftPrefix of Lr1Itemset.t
      | ShiftAccept of Lr1Itemset.t
      | Reduce of Prod.Index.t

    let hash_fold t state =
      match t with
      | ShiftPrefix lr1itemset -> state |> Uns.hash_fold 0L |> Lr1Itemset.hash_fold lr1itemset
      | ShiftAccept lr1itemset -> state |> Uns.hash_fold 1L |> Lr1Itemset.hash_fold lr1itemset
      | Reduce prod_index -> state |> Uns.hash_fold 2L |> Prod.Index.hash_fold prod_index

    let cmp t0 t1 =
      let open Cmp in
      match t0, t1 with
      | ShiftPrefix _, ShiftAccept _
      | ShiftPrefix _, Reduce _
      | ShiftAccept _, Reduce _
        -> Lt
      | ShiftPrefix s0, ShiftPrefix s1
      | ShiftAccept s0, ShiftAccept s1
        -> Lr1Itemset.cmp s0 s1
      | Reduce i0, Reduce i1
        -> Prod.Index.cmp i0 i1
      | ShiftAccept _, ShiftPrefix _
      | Reduce _, ShiftPrefix _
      | Reduce _, ShiftAccept _
        -> Gt

    let pp t formatter =
      match t with
      | ShiftPrefix lr1itemset -> formatter |> Fmt.fmt "ShiftPrefix " |> Lr1Itemset.pp lr1itemset
      | ShiftAccept lr1itemset -> formatter |> Fmt.fmt "ShiftAccept " |> Lr1Itemset.pp lr1itemset
      | Reduce prod_index -> formatter |> Fmt.fmt "Reduce " |> Prod.Index.pp prod_index
  end
  include T
  include Identifiable.Make(T)
end

module Actionset = struct
  type t = (Action.t, Action.cmper_witness) Ordset.t
end

module T = struct
  module Index = Uns
  type t = {
    index: Index.t;
    kernel: Lr1Itemset.t;
    added: Lr1Itemset.t;
  }

  let hash_fold {index; _} state =
    state |> Index.hash_fold index

  let cmp {index=i0; _} {index=i1; _} =
    Index.cmp i0 i1

  let pp {index; kernel; added} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; kernel=" |> Lr1Itemset.pp kernel
    |> Fmt.fmt "; added=" |> Lr1Itemset.pp added
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let fold_until ~init ~f {kernel; added; _} =
  let accum, until = Lr1Itemset.fold_until ~init:(init, false) ~f:(fun (accum, _) lr1item ->
    let accum, until = f accum lr1item in
    (accum, until), until
  ) kernel in
  match until with
  | true -> accum
  | false -> Lr1Itemset.fold_until ~init:accum ~f added

let fold ~init ~f {kernel; added; _} =
  Lr1Itemset.fold ~init:(Lr1Itemset.fold ~init ~f kernel) ~f added

let goto symbol t =
  fold ~init:Lr1Itemset.empty
    ~f:(fun lr1itemset (Lr1Item.{lr0item={prod={rhs_indexes; _} as prod; dot}; _} as lr1item) ->
      match Uns.(dot < Array.length rhs_indexes) &&
            Uns.(Array.get dot rhs_indexes = Symbol.(symbol.index)) with
      | false -> lr1itemset
      | true -> begin
          let lr0item' = Lr0Item.init ~prod ~dot:(succ dot) in
          let lr1item' = Lr1Item.init ~lr0item:lr0item' ~follow:lr1item.follow in
          assert (Lr1Item.is_kernel_item lr1item');
          Lr1Itemset.insert lr1item' lr1itemset
        end
    ) t

let actions symbols t =
  let actions_insert symbol_index action actions = begin
    Ordmap.amend symbol_index ~f:(fun action_set_opt ->
      let action_set' = match action_set_opt with
        | None -> Ordset.singleton (module Action) action
        | Some action_set -> Ordset.insert action action_set
      in
      Some action_set'
    ) actions
  end in
  fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun actions {lr0item={prod={index; rhs_indexes; _}; dot}; follow} ->
      match Uns.(<) dot (Array.length rhs_indexes) with
      (* X :: a·Ab *)
      | true -> begin
          let symbol_index = Array.get dot rhs_indexes in
          let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
          match Symbol.is_token symbol with
          | false -> actions
          | true -> begin
              let goto = goto symbol t in
              let action = match Lr1Itemset.is_accept goto with
                | false -> Action.ShiftPrefix goto
                | true -> Action.ShiftAccept goto
              in
              actions_insert symbol_index action actions
            end
        end
      (* X ::= a· *)
      | false -> begin
          Ordset.fold ~init:actions ~f:(fun actions symbol_index ->
            let action = Action.Reduce index in
            actions_insert symbol_index action actions
          ) follow
        end
    ) t

let gotos symbols t =
  Symbols.nonterms_fold ~init:(Ordmap.empty (module Symbol.Index)) ~f:(fun gotos nonterm ->
    let goto = goto nonterm t in
    match Lr1Itemset.is_empty goto with
    | true -> gotos
    | false -> Ordmap.insert_hlt ~k:nonterm.index ~v:goto gotos
  ) symbols

(* Update closure to incorporate `lr1itemset`. *)
let add_lr1itemset symbols lr1itemset t =
  let rec f symbols lr1itemset t = begin
    match Lr1Itemset.choose lr1itemset with
    | None -> t
    | Some (Lr1Item.{lr0item={prod={rhs_indexes; _} as prod; dot}; follow} as lr1item) -> begin
        let lr1itemset' = Lr1Itemset.remove lr1item lr1itemset in
        match Uns.(dot < Array.length rhs_indexes) with
        | false -> begin
            (* X :: a·Ab *)
            f symbols lr1itemset' t
          end
        | true -> begin
            (* X ::= a· *)
            let rhs_symbol_index = Array.get dot rhs_indexes in
            let rhs_symbol = Symbols.symbol_of_symbol_index rhs_symbol_index symbols in
            match Symbol.is_nonterm rhs_symbol with
            | false -> f symbols lr1itemset' t
            | true -> begin
                let lhs = rhs_symbol in
                let follow' = Lr1Item.first symbols
                    (Lr1Item.init ~lr0item:(Lr0Item.init ~prod ~dot:(succ dot)) ~follow) in
                let lr1itemset', t' = Ordset.fold ~init:(lr1itemset', t)
                  ~f:(fun (lr1itemset, t) prod ->
                    let lr0item = Lr0Item.init ~prod ~dot:0L in
                    let lr1item = Lr1Item.init ~lr0item ~follow:follow' in
                    match Lr1Itemset.mem lr1item t.added with
                    | true -> lr1itemset, t
                    | false -> begin
                        let lr1itemset' = Lr1Itemset.insert lr1item lr1itemset in
                        let added' = Lr1Itemset.insert_hlt lr1item t.added in
                        lr1itemset', {t with added=added'}
                      end
                  ) lhs.prods in
                f symbols lr1itemset' t'
              end
          end
      end
  end in
  f symbols lr1itemset t

(* Merge the kernel represented by `lr1itemset` into `t`'s kernel, then update the closure. *)
let merge symbols lr1itemset t =
  let lr1itemset', kernel' = Lr1Itemset.fold
      ~init:(Lr1Itemset.empty, t.kernel)
      ~f:(fun (lr1itemset, kernel) lr1item ->
        assert (Lr1Item.is_kernel_item lr1item);
        match Lr1Itemset.mem lr1item kernel with
        | true -> lr1itemset, kernel
        | false -> begin
            let lr1itemset' = Lr1Itemset.insert_hlt lr1item lr1itemset in
            let kernel' = Lr1Itemset.insert_hlt lr1item kernel in
            lr1itemset', kernel'
          end
      ) lr1itemset in
  assert (Bool.(=) (Lr1Itemset.is_empty lr1itemset') (Lr1Itemset.(=) t.kernel kernel'));
  match Lr1Itemset.is_empty lr1itemset' with
  | true -> false, t
  | false -> begin
      let t' = add_lr1itemset symbols lr1itemset' {t with kernel=kernel'} in
      true, t'
    end

let union symbols {index=i0; kernel=k0; _} ({index=i1; _} as t) =
  match merge symbols k0 {t with index=Index.min i0 i1} with _, t' -> t'

let init symbols ~index lr1itemset =
  match merge symbols lr1itemset {
    index;
    kernel=Lr1Itemset.empty;
    added=Lr1Itemset.empty;
  } with _, t -> t

let reindex index_map ({index; _} as t) =
  {t with index=Map.get_hlt index index_map}

let next t =
  fold ~init:(Ordset.empty (module Symbol.Index))
    ~f:(fun symbol_indexes Lr1Item.{lr0item={prod={rhs_indexes; _}; dot}; _} ->
      match Uns.(dot < Array.length rhs_indexes) with
      | false -> symbol_indexes
      | true -> begin
          let symbol_index = Array.get dot rhs_indexes in
          Ordset.insert symbol_index symbol_indexes
        end
    ) t
