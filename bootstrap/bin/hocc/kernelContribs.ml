open Basis
open! Basis.Rudiments

module T = struct
  type t = (Lr1Item.t, Contribs.t, Lr1Item.cmper_witness) Ordmap.t

  let hash_fold = Ordmap.hash_fold Contribs.hash_fold

  let cmp = Ordmap.cmp Contribs.cmp

  let pp = Ordmap.pp Contribs.pp

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    List.fmt ~alt ~width (fun (lr1item, contribs) formatter ->
      formatter
      |> Lr1Item.pp_hr symbols lr1item
      |> Fmt.fmt " = "
      |> Contribs.fmt_hr symbols prods ~alt ~width:(width + 4L) contribs
    ) (Ordmap.to_alist t) formatter
end
include T
include Identifiable.Make(T)

let length = Ordmap.length

let equal t0 t1 =
  Ordmap.equal Contribs.equal t0 t1

module Seq = struct
  type container = t
  type elm = Lr1Item.t * Contribs.t
  type t = (Lr1Item.t, Contribs.t, Lr1Item.cmper_witness) Ordmap.Seq.t

  let init = Ordmap.Seq.init
  let length = Ordmap.Seq.length
  let next = Ordmap.Seq.next
  let next_opt = Ordmap.Seq.next_opt
end

let empty = Ordmap.empty (module Lr1Item)

let singleton item contribs =
  Ordmap.singleton (module Lr1Item) ~k:item ~v:contribs

let reindex index_map t =
  Ordmap.map ~f:(fun (_lr1item, contribs) ->
    Contribs.reindex index_map contribs
  ) t

let is_empty = Ordmap.is_empty

let get = Ordmap.get

let amend = Ordmap.amend

let insert item contribs t =
  Ordmap.amend item ~f:(function
    | None -> Some contribs
    | Some contribs_prev -> Some (Contribs.union contribs contribs_prev)
  ) t

let union t0 t1 =
  Ordmap.union ~f:(fun _item contribs0 contribs1 ->
    Contribs.union contribs0 contribs1
  ) t0 t1

let fold_until = Ordmap.fold_until

let fold = Ordmap.fold

let for_any = Ordmap.for_any

let fold2_until = Ordmap.fold2_until

let fold2 = Ordmap.fold2
