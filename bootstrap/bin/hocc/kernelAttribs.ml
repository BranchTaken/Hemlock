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

let reindex index_map t =
  Ordmap.map ~f:(fun (_lr1item, attribs) ->
    Attribs.reindex index_map attribs
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
          let shift_contrib = Contrib.(inter shift conflict) in
          let shift_attrib = Attrib.init ~conflict_state_index ~symbol_index ~conflict
              ~isucc_lr1itemset ~contrib:shift_contrib in
          let has_shift = Contrib.is_empty shift_contrib in
          Lr1Itemset.fold ~init:attribs ~f:(fun attribs isucc_lr1item ->
            match Lr1Itemset.get isucc_lr1item lr1itemset with
            | None -> begin
                match has_shift with
                | false -> attribs
                | true ->
                  Attribs.insert shift_attrib attribs
              end
            | Some {follow; _} -> begin
                match Ordset.mem symbol_index follow with
                | false -> begin
                    match has_shift with
                    | false -> attribs
                    | true ->
                      Attribs.insert shift_attrib attribs
                  end
                | true -> begin
                    let attrib' = Attrib.union shift_attrib attrib in
                    Attribs.insert attrib' attribs
                  end
              end
          ) isucc_lr1itemset
        ) src_lr1item_attribs
    ) t
