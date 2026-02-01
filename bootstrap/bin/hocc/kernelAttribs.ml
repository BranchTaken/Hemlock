open Basis
open! Basis.Rudiments

module T = struct
  type t = (Lr1Item.t, Attribs.t, Lr1Item.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold Attribs.hash_fold

  let cmp = Ordmap.cmp Attribs.cmp

  let pp = Ordmap.pp Attribs.pp

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    List.fmt ~alt ~width (fun (lr1item, attribs) formatter ->
      formatter
      |> Lr1Item.pp_hr symbols lr1item
      |> Fmt.fmt " = "
      |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) attribs
    ) (Ordmap.to_alist t) formatter
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal Attribs.equal t0 t1

module Seq = struct
  type container = t
  type elm = Lr1Item.t * Attribs.t
  type t = (Lr1Item.t, Attribs.t, Lr1Item.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Lr1Item)

let singleton item attribs =
  Ordmap.singleton (module Lr1Item) ~k:item ~v:attribs

let remerge1 remergeable_index_map t =
  Ordmap.map ~f:(fun (_lr1item, attribs) ->
    Attribs.remerge1 remergeable_index_map attribs
  ) t

let is_empty = Ordmap.is_empty

let get = Ordmap.get

let amend = Ordmap.amend

let insert item attribs t =
  Ordmap.amend item ~f:(function
    | None -> Some attribs
    | Some attribs_prev -> Some (Attribs.union attribs attribs_prev)
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _item attribs0 attribs1 ->
    Attribs.union attribs0 attribs1
  ) t0 t1

let merge t0 t1 =
  (* Manually compute the union of `t0` and `t1` such that `strict_superset` is false if the union
   * equals `t1`. The conceptually simpler approach of computing the union via `union` and checking
   * equality of before/after kernel attribs is a lot more expensive for the no-op (equal) case. *)
  Ordmap.fold ~init:(false, t1)
    ~f:(fun (strict_superset, t) (lr1item, attribs) ->
      match Ordmap.get lr1item t1 with
      | None -> true, insert lr1item attribs t
      | Some attribs1 -> begin
          Attribs.fold ~init:(strict_superset, t)
            ~f:(fun (strict_superset, t)
              (Attrib.{conflict_state_index; symbol_index; _} as attrib0) ->
              match Attribs.get ~conflict_state_index ~symbol_index attribs1 with
              | None -> true, insert lr1item (Attribs.singleton attrib0) t
              | Some attrib1 -> begin
                  let attrib = Attrib.diff attrib0 attrib1 in
                  match Attrib.is_empty attrib with
                  | true -> strict_superset, t
                  | false -> true, insert lr1item (Attribs.singleton attrib) t
                end
            ) attribs
        end
    ) t0

(* Not used. *)
let inter t0 t1 =
  match is_empty t0, is_empty t1 with
  | true, _
  | _, true -> empty
  | false, false -> begin
      Ordmap.fold2 ~init:empty ~f:(fun t lr1item_attribs0_opt lr1item_attribs1_opt ->
        match lr1item_attribs0_opt, lr1item_attribs1_opt with
        | Some _, None
        | None, Some _ -> t
        | Some (lr1item, attribs0), Some (_lr1item, attribs1) -> begin
            let attribs = Attribs.inter attribs0 attribs1 in
            match Attribs.is_empty attribs with
            | true -> t
            | false -> Ordmap.insert ~k:lr1item ~v:attribs t
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
      Ordmap.fold2 ~init:empty ~f:(fun t lr1item_attribs0_opt lr1item_attribs1_opt ->
        match lr1item_attribs0_opt, lr1item_attribs1_opt with
        | Some (lr1item, attribs), None -> Ordmap.insert ~k:lr1item ~v:attribs t
        | None, Some _ -> t
        | Some (lr1item, attribs0), Some (_lr1item, attribs1) -> begin
            let attribs = Attribs.diff attribs0 attribs1 in
            match Attribs.is_empty attribs with
            | true -> t
            | false -> Ordmap.insert ~k:lr1item ~v:attribs t
          end
        | None, None -> not_reached ()
      ) t0 t1
    end

let fold_until = Ordmap.fold_until

let fold = Ordmap.fold

let for_any = Ordmap.for_any

let fold2_until = Ordmap.fold2_until

let fold2 = Ordmap.fold2

let attribs lr1itemset t =
  fold ~init:Attribs.empty
    ~f:(fun attribs (_src_lr1item, src_lr1item_attribs) ->
      Attribs.fold ~init:attribs
        ~f:(fun attribs
          (Attrib.{conflict_state_index; symbol_index; conflict; isucc_lr1itemset; contrib} as
            attrib) ->
          assert Contrib.(inter conflict contrib = contrib);
          let has_shift = Contrib.mem_shift conflict in
          let shift_attrib = match Contrib.mem_shift conflict with
            | true -> Attrib.init ~conflict_state_index ~symbol_index ~conflict
                ~isucc_lr1itemset ~contrib:Contrib.shift
            |   false -> Attrib.empty ~conflict_state_index ~symbol_index ~conflict
          in
          Lr1Itemset.fold ~init:attribs ~f:(fun attribs isucc_lr1item ->
            match Lr1Itemset.get isucc_lr1item lr1itemset with
            | None -> begin
                match has_shift with
                | false -> attribs
                | true -> Attribs.insert shift_attrib attribs
              end
            | Some {follow; _} -> begin
                match Bitset.mem symbol_index follow with
                | false -> begin
                    match has_shift with
                    | false -> attribs
                    | true -> Attribs.insert shift_attrib attribs
                  end
                | true -> begin
                    match has_shift with
                    | false -> Attribs.insert attrib attribs
                    | true -> begin
                        let attrib' = Attrib.union shift_attrib attrib in
                        Attribs.insert attrib' attribs
                      end
                  end
              end
          ) isucc_lr1itemset
        ) src_lr1item_attribs
    ) t
