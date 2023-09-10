open Basis
open! Basis.Rudiments

type t = {
  deq: Lr1ItemsetClosure.Index.t Deq.t;
  set: (Lr1ItemsetClosure.Index.t, Lr1ItemsetClosure.Index.cmper_witness) Set.t;
}

let pp {deq; _} =
  Deq.pp Lr1ItemsetClosure.Index.pp deq

let empty = {
  deq=Deq.empty;
  set=Set.empty (module Lr1ItemsetClosure.Index);
}

let length {set; _} =
  Set.length set

let is_empty {set; _} =
  Set.is_empty set

let push lr1itemsetclosure_index {deq; set} =
  assert (not (Set.mem lr1itemsetclosure_index set));
  {
    deq=Deq.push lr1itemsetclosure_index deq;
    set=Set.insert lr1itemsetclosure_index set;
  }

let push_back lr1itemsetclosure_index {deq; set} =
  assert (not (Set.mem lr1itemsetclosure_index set));
  {
    deq=Deq.push_back lr1itemsetclosure_index deq;
    set=Set.insert lr1itemsetclosure_index set;
  }

let pop {deq; set} =
  let lr1itemsetclosure_index, deq' = Deq.pop deq in
  let set' = Set.remove lr1itemsetclosure_index set in
  lr1itemsetclosure_index, {deq=deq'; set=set'}

let mem lr1itemsetclosure_index {set; _} =
  Set.mem lr1itemsetclosure_index set

let set {set; _} =
  set
