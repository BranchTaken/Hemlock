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

let lhs_symbol_indexes {kernel; added; _} =
  let accum = Lr1Itemset.fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun accum Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _} as prod; _}; follow} ->
      match Prod.is_synthetic prod with
      | true -> accum
      | false -> Ordmap.insert ~k:lhs_index ~v:follow accum
    ) kernel
  in
  let accum = Lr1Itemset.fold ~init:accum
      ~f:(fun lhs_symbol_indexes Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _}; _}; follow} ->
        Ordmap.insert ~k:lhs_index ~v:follow lhs_symbol_indexes
      ) added
  in
  accum

let kernel_of_leftmost ~symbol_index ~lhs_index:prod_lhs_index {kernel; added; _} =
  (* Accumulate kernel items with the LHS of prod just past the dot and symbol_index in the follow
   * set.
   *
   * Beware recursive productions, as in the following example involving nested ε productions. All
   * the reduces correspond to the same kernel item, but analysis of B needs to traverse A to reach
   * S. (S' is not reached in this example due to the follow set not containing ⊥.)
   *
   *   S' ::= · S ⊥ {ε}    kernel
   *   S ::= · A    {⊥}    added
   *   S ::= ·      {⊥}    added (reduce)
   *   A ::= · B    {⊥}    added
   *   A ::= ·      {⊥}    added (reduce)
   *   B ::= ·      {⊥}    added (reduce)
   *
   * Mark which symbols have been recursed on, in order to protect against infinite recursion on
   * e.g. `E ::= · E {t}`, as well as on mutually recursive items. *)
  let rec inner kernel added symbol_index prod_lhs_index marks accum = begin
    let marks = Ordset.insert prod_lhs_index marks in
    let accum = Lr1Itemset.fold ~init:accum
        ~f:(fun accum
          (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
          match Uns.( > ) (Array.length rhs_indexes) dot
                && Symbol.Index.( = ) (Array.get dot rhs_indexes) prod_lhs_index
                && Ordset.mem symbol_index follow with
          | false -> accum
          | true -> Lr1Itemset.insert lr1item accum
        ) kernel in
    (* Search the added set for items with the LHS of prod just past the dot and symbol_index in the
     * follow set, and recurse on the items. *)
    let marks, accum = Lr1Itemset.fold ~init:(marks, accum)
      ~f:(fun (marks, accum)
        (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; rhs_indexes; _}; _}; follow}) ->
        match Ordset.mem lhs_index marks with
        | true -> marks, accum
        | false -> begin
            (* The dot is always at position 0 in added items. *)
            match Uns.( > ) (Array.length rhs_indexes) 0L
                  && Symbol.Index.( = ) (Array.get 0L rhs_indexes) prod_lhs_index
                  && Ordset.mem symbol_index follow with
            | false -> marks, accum
            | true -> inner kernel added symbol_index lhs_index marks accum
          end
      ) added in
    marks, accum
  end in
  let _marks, accum = inner kernel added symbol_index prod_lhs_index
      (Ordset.empty (module Symbol.Index)) Lr1Itemset.empty in
  accum

module LeftmostCache = struct
  module K = struct
    module T = struct
      type t = {
        prod_lhs_index: Symbol.Index.t;
        symbol_index: Symbol.Index.t;
      }

      let hash_fold {prod_lhs_index; symbol_index} state =
        state
        |> Symbol.Index.hash_fold prod_lhs_index
        |> Symbol.Index.hash_fold symbol_index

      let cmp {prod_lhs_index=pli0; symbol_index=si0} {prod_lhs_index=pli1; symbol_index=si1} =
        let open Cmp in
        match Symbol.Index.cmp pli0 pli1 with
        | Lt -> Lt
        | Eq -> Symbol.Index.cmp si0 si1
        | Gt -> Gt

      let pp {prod_lhs_index; symbol_index} formatter =
        formatter
        |> Fmt.fmt "{prod_lhs_index=" |> Symbol.Index.pp prod_lhs_index
        |> Fmt.fmt "; symbol_index=" |> Symbol.Index.pp symbol_index
        |> Fmt.fmt "}"

      let init ~prod_lhs_index ~symbol_index =
        {prod_lhs_index; symbol_index}
    end
    include T
    include Identifiable.Make(T)
  end

  type outer = t
  type t = (Symbol.Index.t, (K.t, Lr1Itemset.t, K.cmper_witness) Ordmap.t,
    Symbol.Index.cmper_witness) Ordmap.t

  let empty : t = Ordmap.empty (module Symbol.Index)

  let kernels_of_leftmost prod_lhs_index symbol_indexes lr1itemsetclosure =
    (* Same as outer `kernel_of_leftmost`, except that it processes all symbol indexes in one
     * invocation. Marking is more complicated -- (symbol, symbol_indexes) map rather than symbol
     * set -- because there is no guaranteed that all symbol indexes will be processed in a single
     * recursion on the LHS. *)
    let rec inner kernel added prod_lhs_index inner_lhs_index symbol_indexes marks accum = begin
      let marks = Ordmap.amend inner_lhs_index ~f:(fun lhs_index_opt ->
        match lhs_index_opt with
        | None -> Some symbol_indexes
        | Some symbol_indexes_prev -> Some (Ordset.union symbol_indexes symbol_indexes_prev)
      ) marks in
      let accum = Lr1Itemset.fold ~init:accum
          ~f:(fun accum
            (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
            match Uns.( > ) (Array.length rhs_indexes) dot
                  && Symbol.Index.( = ) (Array.get dot rhs_indexes) inner_lhs_index with
            | false -> accum
            | true -> begin
                Ordset.fold ~init:accum ~f:(fun accum symbol_index ->
                  match Ordset.mem symbol_index follow with
                  | false -> accum
                  | true -> begin
                      let k = K.init ~prod_lhs_index ~symbol_index in
                      Ordmap.amend k ~f:(fun kernel_opt ->
                        match kernel_opt with
                        | None -> Some (Lr1Itemset.singleton lr1item)
                        | Some kernel -> Some (Lr1Itemset.insert lr1item kernel)
                      ) accum
                    end
                ) symbol_indexes
              end
          ) kernel in
      (* Search the added set for items with the prod LHS just past the dot and symbol_index in the
       * follow set. *)
      let found = Lr1Itemset.fold ~init:(Ordmap.empty (module Symbol.Index))
        ~f:(fun found
          (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; rhs_indexes; _}; _}; follow}) ->
          let marked, symbol_indexes = match Ordmap.get lhs_index marks with
            | None -> false, symbol_indexes
            | Some marked_symbol_indexes -> begin
                let symbol_indexes = Ordset.diff symbol_indexes marked_symbol_indexes in
                Ordset.is_empty symbol_indexes, symbol_indexes
              end
          in
          match marked with
          | true -> found
          | false -> begin
              (* The dot is always at position 0 in added items. *)
              match Uns.( > ) (Array.length rhs_indexes) 0L
                    && Symbol.Index.( = ) (Array.get 0L rhs_indexes) inner_lhs_index with
              | false -> found
              | true -> begin
                  let symbol_indexes' = Ordset.inter symbol_indexes follow in
                  match Ordset.is_empty symbol_indexes' with
                  | true -> found
                  | false -> begin
                      Ordmap.amend lhs_index ~f:(fun symbol_indexes_opt ->
                        match symbol_indexes_opt with
                        | None -> Some symbol_indexes'
                        | Some symbol_indexes -> Some (Ordset.union symbol_indexes' symbol_indexes)
                      ) found
                    end
                end
            end
        ) added in
      (* Recurse on the symbols corresponding to items found in the added set search. *)
      let marks, accum = Ordmap.fold ~init:(marks, accum)
        ~f:(fun (marks, accum) (lhs_index, symbol_indexes) ->
          inner kernel added prod_lhs_index lhs_index symbol_indexes marks accum
        ) found in
      marks, accum
    end in
    let {kernel; added; _} = lr1itemsetclosure in
    let marks = Ordmap.empty (module Symbol.Index) in
    let accum = Ordmap.empty (module K) in
    let _marks, accum = inner kernel added prod_lhs_index prod_lhs_index symbol_indexes marks
        accum in
    accum

  let kernel_of_leftmost ~symbol_index ~lhs_index:prod_lhs_index
      ({index=state_index; _} as lr1itemsetclosure) t =
    let state_kernel_cache, t' = match Ordmap.get state_index t with
      | None -> begin
          let state_kernel_cache = Ordmap.fold ~init:(Ordmap.empty (module K))
            ~f:(fun state_kernel_cache (prod_lhs_index, symbol_indexes) ->
              kernels_of_leftmost prod_lhs_index symbol_indexes lr1itemsetclosure
              |> Ordmap.union ~f:(fun _k kernel0 kernel1 ->
                Lr1Itemset.union kernel0 kernel1) state_kernel_cache
            ) (lhs_symbol_indexes lr1itemsetclosure) in
          state_kernel_cache, Ordmap.insert ~k:state_index ~v:state_kernel_cache t
        end
      | Some state_kernel_cache -> state_kernel_cache, t
    in
    let k = K.init ~prod_lhs_index ~symbol_index in
    let kernel = match Ordmap.get k state_kernel_cache with
      | Some kernel -> kernel
      | None -> Lr1Itemset.empty
    in
    kernel, t'
end

(* Update closure to incorporate `lr1itemset`. *)
let add_lr1itemset symbols lr1itemset t =
  let rec f symbols lr1itemset t = begin
    match Lr1Itemset.choose lr1itemset with
    | None -> t
    | Some (Lr1Item.{lr0item={prod={rhs_indexes; _} as prod; dot}; follow} as lr1item) -> begin
        let lr1itemset' = Lr1Itemset.remove lr1item lr1itemset in
        match Uns.(dot < Array.length rhs_indexes) with
        | false -> begin
            (* X ::= a· *)
            f symbols lr1itemset' t
          end
        | true -> begin
            let rhs_symbol_index = Array.get dot rhs_indexes in
            let rhs_symbol = Symbols.symbol_of_symbol_index rhs_symbol_index symbols in
            match Symbol.is_nonterm rhs_symbol with
            | false -> begin
                (* X ::= a·b *)
                f symbols lr1itemset' t
              end
            | true -> begin
                (* X ::= a·Ab *)
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

let remerge symbols remergeable_index_map {index=i0; kernel=k0; _} ({index=i1; _} as t1) =
  let index = match Ordmap.get i0 remergeable_index_map, Ordmap.get i1 remergeable_index_map with
    | Some index, None
    | None, Some index
      -> index
    | Some _, Some _
    | None, None
      -> not_reached ()
  in
  assert Index.(index = min i0 i1);
  match merge symbols k0 {t1 with index} with _, t1' -> t1'

let reindex index_map ({index; _} as t) =
  {t with index=Ordmap.get_hlt index index_map}

let init symbols ~index lr1itemset =
  match merge symbols lr1itemset {
    index;
    kernel=Lr1Itemset.empty;
    added=Lr1Itemset.empty;
  } with _, t -> t

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
