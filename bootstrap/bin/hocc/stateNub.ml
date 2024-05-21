open Basis
open! Basis.Rudiments

module Action = struct
  module T = struct
    type t =
      | ShiftPrefix of Lr1Itemset.t
      | ShiftAccept of Lr1Itemset.t
      | Reduce of Prod.Index.t

    let hash_fold t state =
      match t with
      | ShiftPrefix goto -> state |> Uns.hash_fold 0L |> Lr1Itemset.hash_fold goto
      | ShiftAccept goto -> state |> Uns.hash_fold 1L |> Lr1Itemset.hash_fold goto
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
      | ShiftPrefix goto -> formatter |> Fmt.fmt "ShiftPrefix " |> Lr1Itemset.pp goto
      | ShiftAccept goto -> formatter |> Fmt.fmt "ShiftAccept " |> Lr1Itemset.pp goto
      | Reduce prod_index -> formatter |> Fmt.fmt "Reduce " |> Prod.Index.pp prod_index

    let pp_hr symbols prods t formatter =
      match t with
      | ShiftPrefix goto -> formatter |> Fmt.fmt "ShiftPrefix " |> Lr1Itemset.fmt_hr symbols goto
      | ShiftAccept goto -> formatter |> Fmt.fmt "ShiftAccept " |> Lr1Itemset.fmt_hr symbols goto
      | Reduce prod_index -> begin
          let prod = Prods.prod_of_prod_index prod_index prods in
          formatter
          |> Fmt.fmt "Reduce "
          |> Symbols.pp_prod_hr prod symbols
        end
  end
  include T
  include Identifiable.Make(T)
end

module Actionset = struct
  type t = (Action.t, Action.cmper_witness) Ordset.t

  let resolve symbols prods symbol_index t =
    let prec_of_action symbols prods symbol_index action = begin
      let open Action in
      match action with
      | ShiftPrefix _
      | ShiftAccept _ ->
        (match Symbols.symbol_of_symbol_index symbol_index symbols with Symbol.{prec; _} -> prec)
      | Reduce prod_index ->
        (match Prods.prod_of_prod_index prod_index prods with Prod.{prec; _} -> prec)
    end in
    let assoc_of_action symbols prods symbol_index action = begin
      match prec_of_action symbols prods symbol_index action with
      | None -> None
      | Some {assoc; _} -> assoc
    end in
    match Ordset.length t with
    | 1L -> t
    | _ -> begin
        (* Compute the subset of actions with maximal precedence, if any. Disjoint precedences are
         * incomparable, i.e. there is no maximal precedence in the presence of disjoint
         * precedences. *)
        let max_prec_action_set =
          Ordset.fold_until ~init:(Ordset.empty (module Action))
            ~f:(fun max_prec_action_set action ->
              match Ordset.is_empty max_prec_action_set with
              | true -> Ordset.singleton (module Action) action, false
              | false -> begin
                  let max_prec = prec_of_action symbols prods symbol_index
                      (Ordset.choose_hlt max_prec_action_set) in
                  let action_prec = prec_of_action symbols prods symbol_index action in
                  match max_prec, action_prec with
                  | None, _
                  | _, None -> begin
                      (* Disjoint lack of precedence(s). *)
                      Ordset.empty (module Action), true
                    end
                  | Some max_prec, Some action_prec -> begin
                      match Uns.(=) max_prec.index action_prec.index with
                      | false -> begin
                          match Ordset.mem max_prec.index action_prec.doms with
                          | false -> begin
                              match Ordset.mem action_prec.index max_prec.doms with
                              | false -> begin
                                  (* Disjoint precedence; no conflict resolution possible. *)
                                  Ordset.empty (module Action), true
                                end
                              | true -> begin
                                  (* Action's precedence exceeds current maximal precedence. Replace
                                   * dominated set with the singleton set containing action. *)
                                  Ordset.singleton (module Action) action, false
                                end
                            end
                          | true -> begin
                              (* Current maximal precedence dominates action's precedence. *)
                              max_prec_action_set, false
                            end
                        end
                      | true -> begin
                          (* Precedence equal to current maximal precedence. *)
                          Ordset.insert action max_prec_action_set, false
                        end
                    end
                end
            ) t
        in
        match Ordset.length max_prec_action_set with
        | 0L -> t
        | 1L -> max_prec_action_set
        | _ -> begin
            (* Determine whether the subset of actions with maximal precedence has homogeneous
             * associativity. *)
            let assoc = assoc_of_action symbols prods symbol_index
                (Ordset.choose_hlt max_prec_action_set) in
            let homogeneous = Ordset.fold_until ~init:true ~f:(fun _ action ->
              let action_assoc = assoc_of_action symbols prods symbol_index action in
              match Cmp.is_eq (Option.cmp Assoc.cmp assoc action_assoc) with
              | false -> false, true
              | true -> true, false
            ) max_prec_action_set in
            match homogeneous with
            | false -> t
            | true -> begin
                match assoc with
                | None -> begin
                    (* Resolve a singleton. *)
                    match Ordset.length max_prec_action_set with
                    | 1L -> max_prec_action_set
                    | _ -> t
                  end
                | Some Left -> begin
                    (* Resolve a single reduce action. *)
                    let reduce_action_set = Ordset.fold_until
                        ~init:(Ordset.empty (module Action))
                        ~f:(fun reduce_action_set action ->
                          let open Action in
                          match action with
                          | ShiftPrefix _
                          | ShiftAccept _ -> reduce_action_set, false
                          | Reduce _ -> begin
                              match Ordset.is_empty reduce_action_set with
                              | false -> Ordset.empty (module Action), true
                              | true -> Ordset.singleton (module Action) action, false
                            end
                        ) max_prec_action_set in
                    match Ordset.length reduce_action_set with
                    | 1L -> reduce_action_set
                    | _ -> t
                  end
                | Some Right -> begin
                    (* Resolve a (single) shift action. *)
                    let shift_action_set = Ordset.fold_until
                        ~init:(Ordset.empty (module Action))
                        ~f:(fun shift_action_set action ->
                          let open Action in
                          match action with
                          | ShiftPrefix _
                          | ShiftAccept _ -> Ordset.singleton (module Action) action, true
                          | Reduce _ -> shift_action_set, false
                        ) max_prec_action_set in
                    match Ordset.length shift_action_set with
                    | 1L -> shift_action_set
                    | _ -> t
                  end
              end
          end
      end
end

module T = struct
  module Index = Lr1ItemsetClosure.Index
  type t = {
    lr1itemsetclosure: Lr1ItemsetClosure.t;
    isocores_sn: uns;
    isocore_set_sn: uns;
    transit_attribs_lst: TransitAttribs.t list;
    attribs: Attribs.t;
  }

  let hash_fold {lr1itemsetclosure; _} state =
    state |> Lr1ItemsetClosure.hash_fold lr1itemsetclosure

  let cmp {lr1itemsetclosure=c0; _} {lr1itemsetclosure=c1; _} =
    Lr1ItemsetClosure.cmp c0 c1

  let pp {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst; attribs} formatter =
    formatter
    |> Fmt.fmt "{lr1itemsetclosure=" |> Lr1ItemsetClosure.pp  lr1itemsetclosure
    |> Fmt.fmt "; isocores_sn=" |> Uns.pp isocores_sn
    |> Fmt.fmt "; isocore_set_sn=" |> Uns.pp isocore_set_sn
    |> Fmt.fmt "; transit_attribs_lst=" |> List.pp TransitAttribs.pp transit_attribs_lst
    |> Fmt.fmt "; attribs=" |> Attribs.pp attribs
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init symbols ~index ~isocores_sn ~isocore_set_sn GotoNub.{goto; transit_attribs; attribs; _} =
  let lr1itemsetclosure = Lr1ItemsetClosure.init symbols ~index goto in
  {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst=[transit_attribs]; attribs}

let reindex index_map
    {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst; attribs} =
  let lr1itemsetclosure = Lr1ItemsetClosure.reindex index_map lr1itemsetclosure in
  let transit_attribs_lst = List.map ~f:(fun transit_attribs ->
    TransitAttribs.reindex index_map transit_attribs
  ) transit_attribs_lst in
  let attribs = Attribs.reindex index_map attribs in
  {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst; attribs}

let index {lr1itemsetclosure; _} =
  lr1itemsetclosure.index

let isocores_sn {isocores_sn; _} =
  isocores_sn

let isocore_set_sn {isocore_set_sn; _} =
  isocore_set_sn

let merge symbols GotoNub.{goto; transit_attribs; _}
  {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst; attribs} =
  let merged, (Lr1ItemsetClosure.{kernel=lr1itemset; _} as lr1itemsetclosure) =
    Lr1ItemsetClosure.merge symbols goto lr1itemsetclosure in
  let transit_attribs_lst = transit_attribs :: transit_attribs_lst in
  let attribs = match merged with
    | false -> attribs (* No-op merge means no change in attribs. *)
    | true -> begin
        List.fold ~init:Attribs.empty ~f:(fun attribs transit_attribs ->
          Attribs.union (TransitAttribs.attribs lr1itemset transit_attribs) attribs
        ) transit_attribs_lst
      end
  in
  merged, {lr1itemsetclosure; isocores_sn; isocore_set_sn; transit_attribs_lst; attribs}

let next {lr1itemsetclosure; _} =
  Lr1ItemsetClosure.next lr1itemsetclosure

let goto symbol {lr1itemsetclosure; _} =
  Lr1ItemsetClosure.goto symbol lr1itemsetclosure

let actions symbols {lr1itemsetclosure; _} =
  Lr1ItemsetClosure.actions symbols lr1itemsetclosure
  |> Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun actions (symbol_index, action_set) ->
      let action_set' = Ordset.fold ~init:(Ordset.empty (module Action))
        ~f:(fun action_set action ->
          Ordset.insert (match action with
            | Lr1ItemsetClosure.Action.ShiftPrefix goto -> Action.ShiftPrefix goto
            | Lr1ItemsetClosure.Action.ShiftAccept goto -> Action.ShiftAccept goto
            | Lr1ItemsetClosure.Action.Reduce prod_index -> Action.Reduce prod_index
          ) action_set
        ) action_set
      in
      Ordmap.insert ~k:symbol_index ~v:action_set' actions
    )

let gotos symbols {lr1itemsetclosure; _} =
  Lr1ItemsetClosure.gotos symbols lr1itemsetclosure
  |> Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index)) ~f:(fun gotos (nonterm_index, goto) ->
    Ordmap.insert_hlt ~k:nonterm_index ~v:goto gotos
  )

let resolve symbols prods actions =
  Ordmap.fold ~init:(Ordmap.empty (module Symbol.Index))
    ~f:(fun actions (symbol_index, action_set) ->
      Ordmap.insert_hlt ~k:symbol_index ~v:(Actionset.resolve symbols prods symbol_index action_set)
        actions
    ) actions

let compat_lr1 GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_lr1 goto kernel

let compat_ielr1 ~resolve symbols prods GotoNub.{attribs=o_attribs; _} {attribs=t_attribs; _} =
  Attribs.fold2_until ~init:true
    ~f:(fun _compat attrib_opt0 attrib_opt1 ->
      let o_attrib, t_attrib = match attrib_opt0, attrib_opt1 with
        | Some o_attrib, Some t_attrib -> o_attrib, t_attrib
        | Some (Attrib.{conflict_state_index; symbol_index; conflict; _} as o_attrib), None ->
          o_attrib, Attrib.empty ~conflict_state_index ~symbol_index ~conflict
        | None, Some (Attrib.{conflict_state_index; symbol_index; conflict; _} as t_attrib) ->
          Attrib.empty ~conflict_state_index ~symbol_index ~conflict, t_attrib
        | None, None -> not_reached ()
      in
      let compat = Attrib.compat_ielr1 ~resolve symbols prods o_attrib t_attrib in
      compat, not compat
    ) o_attribs t_attribs

let compat_pgm1 GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_pgm1 goto kernel

let compat_lalr1 GotoNub.{goto; _} {lr1itemsetclosure={kernel; _}; _} =
  Lr1Itemset.compat_lalr1 goto kernel
