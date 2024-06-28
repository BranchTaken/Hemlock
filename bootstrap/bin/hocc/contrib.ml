open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    (* At most one shift can be contributed, and its concrete value is not important. Track its
     * presence in the contribution via a separate boolean to simplify membership querying. *)
    shift: bool;
    (* Any number of reduce actions can be contributed. Multiple contributions from the same state
     * can only happen due to an inherent conflict in the grammar, whereas differing reduce actions
     * contributed by separate states may instead be due to LALR(1) inadequacy. Track reductions by
     * their associated productions to avoid dependency on any particular `Action.t`. *)
    reduce: (Prod.Index.t, Prod.Index.cmper_witness) Ordset.t;
  }

  let hash_fold {shift; reduce} state =
    state |> Bool.hash_fold shift |> Ordset.hash_fold reduce

  let cmp {shift=s0; reduce=r0} {shift=s1; reduce=r1} =
    let open Cmp in
    match Bool.cmp s0 s1 with
    | Lt -> Lt
    | Eq -> Ordset.cmp r0 r1
    | Gt -> Gt

  let equal {shift=s0; reduce=r0} {shift=s1; reduce=r1} =
    Bool.(s0 = s1) && Ordset.equal r0 r1

  let pp {shift; reduce} formatter =
    formatter
    |> Fmt.fmt "{shift=" |> Bool.pp shift
    |> Fmt.fmt "; reduce=" |> Ordset.pp reduce
    |> Fmt.fmt "}"

  let pp_hr symbols prods {shift; reduce} formatter =
    formatter
    |> Fmt.fmt (match shift with false -> "{" | true -> "{Shift")
    |> (fun formatter ->
      match Ordset.is_empty reduce with
      | true -> formatter
      | false -> begin
          formatter
          |> Fmt.fmt (match shift with false -> "" | true -> "; ")
          |> (fun formatter ->
            Ordset.foldi ~init:formatter ~f:(fun i formatter prod_index ->
              let prod = Prods.prod_of_prod_index prod_index prods in
              formatter
              |> Fmt.fmt (match i with 0L -> "" | _ -> "; ")
              |> Fmt.fmt "Reduce ["
              |> Symbols.pp_prod_hr prod symbols
              |> Fmt.fmt "]"
            ) reduce
          )
        end
    )
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let length {shift; reduce} =
  Bool.to_uns shift + (Ordset.length reduce)

let empty = {
  shift=false;
  reduce=Ordset.empty (module Prod.Index)
}

let is_empty {shift; reduce} =
  (not shift) && Ordset.is_empty reduce

let shift = {
  shift=true;
  reduce=Ordset.empty (module Prod.Index);
}

let init_reduce prod_index =
  {shift=false; reduce=Ordset.singleton (module Prod.Index) prod_index}

let mem_shift {shift; _} =
  shift

let reduces {reduce; _} =
  reduce

let union {shift=s0; reduce=r0} {shift=s1; reduce=r1} =
  {shift=s0 || s1; reduce=Ordset.union r0 r1}

let inter {shift=s0; reduce=r0} {shift=s1; reduce=r1} =
  {shift=s0 && s1; reduce=Ordset.inter r0 r1}

let diff {shift=s0; reduce=r0} {shift=s1; reduce=r1} =
  {shift=s0 && (not s1); reduce=Ordset.diff r0 r1}

let resolve symbols prods symbol_index t =
  let prec_of_shift symbols symbol_index = begin
    match Symbols.symbol_of_symbol_index symbol_index symbols with Symbol.{prec; _} -> prec
  end in
  let prec_of_reduce prods prod_index = begin
    match Prods.prod_of_prod_index prod_index prods with Prod.{prec; _} -> prec
  end in
  let assoc_of_shift symbols symbol_index = begin
    match prec_of_shift symbols symbol_index with
    | None -> None
    | Some {assoc; _} -> assoc
  end in
  let assoc_of_reduce prods prod_index = begin
    match prec_of_reduce prods prod_index with
    | None -> None
    | Some {assoc; _} -> assoc
  end in
  match length t with
  | 0L
  | 1L -> t
  | _ -> begin
      (* Compute the subset of actions with maximal precedence, if any. Disjoint precedences are
       * incomparable, i.e. there is no maximal precedence in the presence of disjoint precedences.
      *)
      let max_prec_contrib = Ordset.fold_until ~init:(inter shift t)
        ~f:(fun max_prec_contrib prod_index ->
          match is_empty max_prec_contrib with
          | true -> init_reduce prod_index, false
          | false -> begin
              let max_prec = match mem_shift max_prec_contrib with
                | true -> prec_of_shift symbols symbol_index
                | false -> prec_of_reduce prods (Ordset.choose_hlt max_prec_contrib.reduce)
              in
              let reduce_prec = prec_of_reduce prods prod_index in
              match max_prec, reduce_prec with
              | None, _
              | _, None -> begin
                  (* Disjoint lack of precedence(s). *)
                  empty, true
                end
              | Some max_prec, Some reduce_prec -> begin
                  match Uns.(=) max_prec.index reduce_prec.index with
                  | false -> begin
                      match Ordset.mem max_prec.index reduce_prec.doms with
                      | false -> begin
                          match Ordset.mem reduce_prec.index max_prec.doms with
                          | false -> begin
                              (* Disjoint precedence; no conflict resolution possible. *)
                              empty, true
                            end
                          | true -> begin
                              (* Reduction's precedence exceeds current maximal precedence. Replace
                               * dominated set with the singleton set containing reduction. *)
                              init_reduce prod_index, false
                            end
                        end
                      | true -> begin
                          (* Current maximal precedence dominates reduction's precedence. *)
                          max_prec_contrib, false
                        end
                    end
                  | true -> begin
                      (* Precedence equal to current maximal precedence. *)
                      let reduce_contrib = init_reduce prod_index in
                      union reduce_contrib max_prec_contrib, false
                    end
                end
            end
        ) t.reduce in
      match length max_prec_contrib with
      | 0L -> t
      | 1L -> max_prec_contrib
      | _ -> begin
          (* Determine whether the subset of actions with maximal precedence has homogeneous
           * associativity. *)
          let assoc = match mem_shift max_prec_contrib with
            | true -> assoc_of_shift symbols symbol_index
            | false -> assoc_of_reduce prods (Ordset.choose_hlt max_prec_contrib.reduce)
          in
          let homogeneous = Ordset.fold_until ~init:true ~f:(fun _ prod_index ->
            let reduce_assoc = assoc_of_reduce prods prod_index in
            match Cmp.is_eq (Option.cmp Assoc.cmp assoc reduce_assoc) with
            | false -> false, true
            | true -> true, false
          ) max_prec_contrib.reduce in
          match homogeneous with
          | false -> t
          | true -> begin
              match assoc with
              | None -> begin
                  (* Resolve a singleton. *)
                  match length max_prec_contrib with
                  | 1L -> max_prec_contrib (* Not reached due to earlier length check. *)
                  | _ -> t
                end
              | Some Left -> begin
                  (* Resolve a single reduce action. *)
                  match Ordset.length max_prec_contrib.reduce with
                  | 1L -> {max_prec_contrib with shift=false}
                  | _ -> t
                end
              | Some Right -> begin
                  match mem_shift max_prec_contrib with
                  | true -> shift
                  | _ -> t
                end
            end
        end
    end
