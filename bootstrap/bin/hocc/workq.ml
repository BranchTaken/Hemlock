open Basis
open! Basis.Rudiments

type ('a, 'cmp) t = {
  cmper: ('a, 'cmp) Cmper.t;
  deq: 'a Deq.t;
  set: ('a, 'cmp) Set.t;
}

type ('a, 'cmp) cmper = (module Cmper.SMono with type t = 'a and type cmper_witness = 'cmp)

(* Extract cmper from first-class module compatible with Cmper.SMono. *)
let m_cmper (type k cmp) ((module M) : (k, cmp) cmper) =
  M.cmper

let pp {cmper; deq; _} =
  Deq.pp cmper.pp deq

let empty m = {
  cmper=m_cmper m;
  deq=Deq.empty;
  set=Set.empty m;
}

let length {set; _} =
  Set.length set

let is_empty {set; _} =
  Set.is_empty set

let push elm {cmper; deq; set} =
  assert (not (Set.mem elm set));
  {
    cmper;
    deq=Deq.push elm deq;
    set=Set.insert elm set;
  }

let push_back elm {cmper; deq; set} =
  assert (not (Set.mem elm set));
  {
    cmper;
    deq=Deq.push_back elm deq;
    set=Set.insert elm set;
  }

let pop {cmper; deq; set} =
  let elm, deq' = Deq.pop deq in
  let set' = Set.remove elm set in
  elm, {cmper; deq=deq'; set=set'}

let mem elm {set; _} =
  Set.mem elm set

let set {set; _} =
  set
