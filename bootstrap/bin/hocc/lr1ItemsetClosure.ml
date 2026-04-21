open Basis
open Basis.Rudiments

module T = struct
  module Index = Uns
  type t = {
    index: Index.t;
    kernel: Lr1Itemset.t;
    added: Lr1Itemset.t lazy_t;
  }

  let hash_fold {index; _} state =
    state |> Index.hash_fold index

  let cmp {index=i0; _} {index=i1; _} =
    Index.cmp i0 i1

  let pp {index; kernel; added} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; kernel=" |> Lr1Itemset.pp kernel
    |> Fmt.fmt "; added=" |> Lr1Itemset.pp (Lazy.force added)
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let added_impl symbols kernel =
  let rec f symbols lr1itemset added = begin
    match Lr1Itemset.choose lr1itemset with
    | None -> added
    | Some (Lr1Item.{lr0item={prod={rhs_indexes; _} as prod; dot}; follow} as lr1item) -> begin
        let lr1itemset' = Lr1Itemset.remove lr1item lr1itemset in
        match Uns.(dot < Array.length rhs_indexes) with
        | false -> begin
            (* X ::= a· *)
            f symbols lr1itemset' added
          end
        | true -> begin
            let rhs_symbol_index = Array.get dot rhs_indexes in
            let rhs_symbol = Symbols.symbol_of_symbol_index rhs_symbol_index symbols in
            match Symbol.is_nonterm rhs_symbol with
            | false -> begin
                (* X ::= a·b *)
                f symbols lr1itemset' added
              end
            | true -> begin
                (* X ::= a·Ab *)
                let lhs = rhs_symbol in
                let follow' = Lr1Item.first symbols
                    (Lr1Item.init ~lr0item:(Lr0Item.init ~prod ~dot:(succ dot)) ~follow) in
                let lr1itemset', added' = Ordset.fold ~init:(lr1itemset', added)
                  ~f:(fun (lr1itemset, added) prod ->
                    let lr0item = Lr0Item.init ~prod ~dot:0L in
                    let lr1item = Lr1Item.init ~lr0item ~follow:follow' in
                    match Lr1Itemset.mem lr1item added with
                    | true -> lr1itemset, added
                    | false -> begin
                        let lr1itemset' = Lr1Itemset.insert lr1item lr1itemset in
                        let added' = Lr1Itemset.insert_hlt lr1item added in
                        lr1itemset', added'
                      end
                  ) lhs.prods in
                f symbols lr1itemset' added'
              end
          end
      end
  end in
  f symbols kernel Lr1Itemset.empty

let merge symbols lr1itemset t =
  (* Merge the kernel represented by `lr1itemset` into `t`'s kernel, then update the lazy closure
   * computation if necessary. *)
  let merged, kernel = Lr1Itemset.fold
      ~init:(false, t.kernel)
      ~f:(fun (merged, kernel) lr1item ->
        assert (Lr1Item.is_kernel_item lr1item);
        match Lr1Itemset.mem lr1item kernel with
        | true -> merged, kernel
        | false -> true, Lr1Itemset.insert_hlt lr1item kernel
      ) lr1itemset in
  match merged with
  | false -> false, t
  | true -> true, {t with kernel; added=lazy (added_impl symbols kernel)}

let remerge symbols {index=i0; kernel=k0; _} ({index=i1; _} as t1) =
  assert StateIndex.(i0 > i1);
  match merge symbols k0 t1 with _, t1' -> t1'

let reindex state_index_map ({index; _} as t) =
  {t with index=StateIndexMap.reindexed_state_index index state_index_map}

let init symbols ~index lr1itemset =
  match merge symbols lr1itemset {
    index;
    kernel=Lr1Itemset.empty;
    added=lazy (added_impl symbols Lr1Itemset.empty);
  } with _, t -> t

let added {added; _} =
  Lazy.force added

let fold ~init ~f {kernel; added; _} =
  Lr1Itemset.fold ~init:(Lr1Itemset.fold ~init ~f kernel) ~f (Lazy.force added)

let gotos_impl symbols symbol_is t =
  fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun gotos Lr1Item.{lr0item={prod={rhs_indexes; _} as prod; dot}; follow} ->
      match Uns.(dot < Array.length rhs_indexes) with
      | false -> gotos
      | true -> begin
          let symbol_index = Array.get dot rhs_indexes in
          let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
          match symbol_is symbol with
          | false -> gotos
          | true -> begin
              let lr0item = Lr0Item.init ~prod ~dot:(succ dot) in
              let lr1item = Lr1Item.init ~lr0item ~follow in
              assert (Lr1Item.is_kernel_item lr1item);
              Ordmap.amend symbol_index ~f:(fun goto_opt ->
                match goto_opt with
                | None -> Some (Lr1Itemset.singleton lr1item)
                | Some goto -> Some (Lr1Itemset.insert lr1item goto)
              ) gotos
            end
        end
    ) t

let token_gotos symbols t =
  gotos_impl symbols Symbol.is_token t

let nonterm_gotos symbols t =
  gotos_impl symbols Symbol.is_nonterm t

let fold_next symbols ~init ~f t =
  Ordmap.fold ~init:(Ordmap.fold ~init ~f (token_gotos symbols t)) ~f (nonterm_gotos symbols t)

let fold_until ~init ~f {kernel; added; _} =
  let accum, until = Lr1Itemset.fold_until ~init:(init, false) ~f:(fun (accum, _) lr1item ->
    let accum, until = f accum lr1item in
    (accum, until), until
  ) kernel in
  match until with
  | true -> accum
  | false -> Lr1Itemset.fold_until ~init:accum ~f (Lazy.force added)

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
    let marks = Bitset.insert prod_lhs_index marks in
    let accum = Lr1Itemset.fold ~init:accum
        ~f:(fun accum
          (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
          match Uns.( > ) (Array.length rhs_indexes) dot
                && Symbol.Index.( = ) (Array.get dot rhs_indexes) prod_lhs_index
                && Bitset.mem symbol_index follow with
          | false -> accum
          | true -> Lr1Itemset.insert lr1item accum
        ) kernel in
    (* Search the added set for items with the LHS of prod just past the dot and symbol_index in the
     * follow set, and recurse on the items. *)
    let marks, accum = Lr1Itemset.fold ~init:(marks, accum)
      ~f:(fun (marks, accum)
        (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; rhs_indexes; _}; _}; follow}) ->
        match Bitset.mem lhs_index marks with
        | true -> marks, accum
        | false -> begin
            (* The dot is always at position 0 in added items. *)
            match Uns.( > ) (Array.length rhs_indexes) 0L
                  && Symbol.Index.( = ) (Array.get 0L rhs_indexes) prod_lhs_index
                  && Bitset.mem symbol_index follow with
            | false -> marks, accum
            | true -> inner kernel added symbol_index lhs_index marks accum
          end
      ) added in
    marks, accum
  end in
  let _marks, accum = inner kernel (Lazy.force added) symbol_index prod_lhs_index Bitset.empty
    Lr1Itemset.empty in
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
     * set -- because there is no guarantee that all symbol indexes will be processed in a single
     * recursion on the LHS. *)
    let rec inner kernel added prod_lhs_index inner_lhs_index symbol_indexes marks accum = begin
      let marks = Ordmap.amend inner_lhs_index ~f:(fun lhs_index_opt ->
        match lhs_index_opt with
        | None -> Some symbol_indexes
        | Some symbol_indexes_prev -> Some (Bitset.union symbol_indexes symbol_indexes_prev)
      ) marks in
      let accum = Lr1Itemset.fold ~init:accum
          ~f:(fun accum
            (Lr1Item.{lr0item=Lr0Item.{prod=Prod.{rhs_indexes; _}; dot}; follow} as lr1item) ->
            match Uns.( > ) (Array.length rhs_indexes) dot
                  && Symbol.Index.( = ) (Array.get dot rhs_indexes) inner_lhs_index with
            | false -> accum
            | true -> begin
                Bitset.fold ~init:accum ~f:(fun accum symbol_index ->
                  match Bitset.mem symbol_index follow with
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
                let symbol_indexes = Bitset.diff symbol_indexes marked_symbol_indexes in
                Bitset.is_empty symbol_indexes, symbol_indexes
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
                  let symbol_indexes' = Bitset.inter symbol_indexes follow in
                  match Bitset.is_empty symbol_indexes' with
                  | true -> found
                  | false -> begin
                      Ordmap.amend lhs_index ~f:(fun symbol_indexes_opt ->
                        match symbol_indexes_opt with
                        | None -> Some symbol_indexes'
                        | Some symbol_indexes -> Some (Bitset.union symbol_indexes' symbol_indexes)
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
    let _marks, accum = inner kernel (Lazy.force added) prod_lhs_index prod_lhs_index symbol_indexes
      marks accum in
    accum

  (* Return a map of all LHS symbols in `t` to their corresponding items' follow sets. *)
  let lhs_symbol_indexes {kernel; added; _} =
    let accum = Lr1Itemset.fold ~init:(Ordmap.empty (module Symbol.Index))
      ~f:(fun accum Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _} as prod; _}; follow} ->
        match Prod.is_synthetic prod with
        | true -> accum
        | false -> begin
            Ordmap.amend lhs_index ~f:(fun follow_opt ->
              match follow_opt with
              | None -> Some follow
              | Some follow_existing -> Some (Bitset.union follow follow_existing)
            ) accum
          end
      ) kernel
    in
    let accum = Lr1Itemset.fold ~init:accum
        ~f:(fun lhs_symbol_indexes
          Lr1Item.{lr0item=Lr0Item.{prod=Prod.{lhs_index; _}; _}; follow} ->
          Ordmap.amend lhs_index ~f:(fun follow_opt ->
            match follow_opt with
            | None -> Some follow
            | Some follow_existing -> Some (Bitset.union follow follow_existing)
          ) lhs_symbol_indexes
        ) (Lazy.force added)
    in
    accum

  let kernel_of_leftmost ~symbol_index ~lhs_index:prod_lhs_index
      ({index=state_index; _} as lr1itemsetclosure) t =
    let state_kernel_cache, t' = match Ordmap.get state_index t with
      | None -> begin
          let state_kernel_cache = Ordmap.fold ~init:(Ordmap.empty (module K))
            ~f:(fun state_kernel_cache (prod_lhs_index, symbol_indexes) ->
              kernels_of_leftmost prod_lhs_index symbol_indexes lr1itemsetclosure
              |> Ordmap.union ~vunion:(fun _k kernel0 kernel1 ->
                Lr1Itemset.union kernel0 kernel1) state_kernel_cache
            ) (lhs_symbol_indexes lr1itemsetclosure) in
          state_kernel_cache, Ordmap.insert_hlt ~k:state_index ~v:state_kernel_cache t
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
