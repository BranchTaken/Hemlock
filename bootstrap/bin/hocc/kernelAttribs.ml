open Basis
open! Basis.Rudiments

module T = struct
  type t = (Lr0Item.t, Attribs.t, Lr0Item.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold Attribs.hash_fold

  let cmp = Ordmap.cmp Attribs.cmp

  let pp = Ordmap.pp Attribs.pp

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    List.fmt ~alt ~width (fun (lr0item, attribs) formatter ->
      formatter
      |> Lr0Item.pp_hr symbols lr0item
      |> Fmt.fmt " = "
      |> Attribs.fmt_hr symbols prods ~alt ~width:(width + 4L) attribs
    ) (Ordmap.to_alist t) formatter
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal ~vequal:(fun _k t0 t1 -> Attribs.equal t0 t1) t0 t1

module Seq = struct
  type container = t
  type elm = Lr0Item.t * Attribs.t
  type t = (Lr0Item.t, Attribs.t, Lr0Item.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Lr0Item)

let singleton lr0item attribs =
  Ordmap.singleton (module Lr0Item) ~k:lr0item ~v:attribs

let is_empty = Ordmap.is_empty

let get = Ordmap.get

let amend = Ordmap.amend

let insert lr0item attribs t =
  Ordmap.amend lr0item ~f:(function
    | None -> Some attribs
    | Some attribs_prev -> Some (Attribs.union attribs attribs_prev)
  ) t

let union t0 t1 =
  Ordmap.union ~vunion:(fun _lr0item attribs0 attribs1 ->
    Attribs.union attribs0 attribs1
  ) t0 t1

(* Not used. *)
let merge t0 t1 =
  (* Manually compute the union of `t0` and `t1` such that `strict_superset` is false if the union
   * equals `t1`. The conceptually simpler approach of computing the union via `union` and checking
   * equality of before/after kernel attribs is a lot more expensive for the no-op (equal) case. *)
  Ordmap.fold ~init:(false, t1)
    ~f:(fun (strict_superset, t) (lr0item, attribs0) ->
      match Ordmap.get lr0item t1 with
      | None -> true, insert lr0item attribs0 t
      | Some attribs1 -> begin
          Attribs.fold ~init:(strict_superset, t)
            ~f:(fun (strict_superset, t)
              (Attrib.{conflict_state_index; symbol_index; _} as attrib0) ->
              match Attribs.get ~conflict_state_index ~symbol_index attribs1 with
              | None -> true, insert lr0item (Attribs.singleton attrib0) t
              | Some attrib1 -> begin
                  let attrib = Attrib.diff attrib0 attrib1 in
                  match Attrib.is_empty attrib with
                  | true -> strict_superset, t
                  | false -> true, insert lr0item (Attribs.singleton attrib) t
                end
            ) attribs0
        end
    ) t0

(* Not used. *)
let inter t0 t1 =
  Ordmap.inter ~vinter:(fun _lr0item attribs0 attribs1 ->
    let attribs = Attribs.inter attribs0 attribs1 in
    match Attribs.is_empty attribs with
    | true -> None
    | false -> Some attribs
  ) t0 t1

(* Not used. *)
let diff t0 t1 =
  Ordmap.diff ~vdiff:(fun _lr0item attribs0 attribs1 ->
    let attribs = Attribs.diff attribs0 attribs1 in
    match Attribs.is_empty attribs with
    | true -> None
    | false -> Some attribs
  ) t0 t1

let fold_until = Ordmap.fold_until

let fold = Ordmap.fold

let for_any = Ordmap.for_any

let fold2_until = Ordmap.fold2_until

let fold2 = Ordmap.fold2

let goto_attribs kernel t =
  fold ~init:(empty, Attribs.empty)
    ~f:(fun (kernel_attribs, attribs) (_src_lr0item, src_attribs) ->
      Attribs.fold ~init:(kernel_attribs, attribs)
        ~f:(fun (kernel_attribs, attribs) (Attrib.{isucc_lr1itemset; _} as attrib) ->
          let isucc_lr1itemset = Lr1Itemset.inter kernel isucc_lr1itemset in
          let kernel_attribs, attribs = match Lr1Itemset.is_empty isucc_lr1itemset with
            | true -> kernel_attribs, attribs
            | false -> begin
                let kernel_attribs = Lr1Itemset.fold ~init:kernel_attribs
                    ~f:(fun kernel_attribs ({lr0item; _} as isucc_lr1item) ->
                      let attrib =
                        {attrib with isucc_lr1itemset=Lr1Itemset.singleton isucc_lr1item} in
                      insert lr0item (Attribs.singleton attrib) kernel_attribs
                    ) isucc_lr1itemset
                in
                let attribs = Attribs.insert {attrib with isucc_lr1itemset} attribs in
                kernel_attribs, attribs
              end
          in
          kernel_attribs, attribs
        ) src_attribs
    ) t
