open Basis
open! Basis.Rudiments

type ('a, 'cmp) t = {
  cmper: ('a, 'cmp) Cmper.t;
  q: (i64, 'a, I64.cmper_witness) Ordmap.t;
  set: ('a, 'cmp) Set.t;
}

type ('a, 'cmp) cmper = (module Cmper.SMono with type t = 'a and type cmper_witness = 'cmp)

(* Extract cmper from first-class module compatible with Cmper.SMono. *)
let m_cmper (type k cmp) ((module M) : (k, cmp) cmper) =
  M.cmper

let pp {cmper; q; _} =
  Ordmap.pp cmper.pp q

let empty m = {
  cmper=m_cmper m;
  q=Ordmap.empty (module I64);
  set=Set.empty m;
}

let length {set; _} =
  Set.length set

let is_empty {set; _} =
  Set.is_empty set

let push elm {cmper; q; set} =
  assert (not (Set.mem elm set));
  let k = match Ordmap.length q with
    | 0L -> I64.zero
    | _ -> begin
        let kmin, _elm = Ordmap.nth 0L q in
        I64.pred kmin
      end
  in
  {
    cmper;
    q=Ordmap.insert_hlt ~k ~v:elm q;
    set=Set.insert elm set;
  }

let push_back elm {cmper; q; set} =
  assert (not (Set.mem elm set));
  let k = match Ordmap.length q with
    | 0L -> I64.zero
    | l -> begin
        let kmax, _elm = Ordmap.nth (pred l) q in
        I64.succ kmax
      end
  in
  {
    cmper;
    q=Ordmap.insert_hlt ~k ~v:elm q;
    set=Set.insert elm set;
  }

let pop {cmper; q; set} =
  let k, elm = Ordmap.nth 0L q in
  let q' = Ordmap.remove_hlt k q in
  let set' = Set.remove elm set in
  elm, {cmper; q=q'; set=set'}

let mem elm {set; _} =
  Set.mem elm set

let set {set; _} =
  set
