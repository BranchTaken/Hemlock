(** Collection of state nub remergeability metadata, used to incrementally record whether state nubs
    are remergeable, and finally to construct a canonical remerging index map. *)

open! Basis
open! Basis.Rudiments

(** Pairwise relationship between isocoric states. *)
type rel =
  | Unknown (* No known relationship. *)
  | Distinct (* Distinct subgraph features prevent remerging. *)
  | Mergeable (* Remergeable. *)

type t

val fmt: ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** Formatter which outputs remergeable isocore sets in human-readable form. *)

val empty: t
(** [empty] returns an empty [t]. *)

val rel: StateNub.t -> StateNub.t -> t -> rel
(** [rel statenub0 statenub1] returns the relation between isocoric state nubs. *)

val mergeable_set: StateNub.t -> t -> (StateNub.t, StateNub.cmper_witness) Ordset.t
(** [mergeable_set statenub t] returns the set of state nubs with which [statenub] a member of, or
    an empty set if [statenub] is not known to be mergeable with any other state nubs. *)

val subgraph_size: t -> uns
(** [subgraph_size t] returns the number of states in each of the two subgraphs currently being
    considered for remergeability. *)

val expand: StateNub.t -> StateNub.t -> t -> t
(** [expand statenub0 statenub1] expands the subgraphs being explored by tentatively recording a
    [Remergeable] relationship between [statenub0] and [statenub1]. *)

val distinct: (StateNub.t * StateNub.t) list -> t -> t
(** [distinct spines t] concludes subgraph exploration with a determination that the subgraphs are
    distinct, and therefore the spines (i.e. paths from exploration roots to distinct state pair)
    are distinct. The spines' relationships transition to [Distinct] and all other tentative
    relationships revert to [Unknown]. *)

val mergeable: t -> t
(** [mergeable t] concludes subgraph exploration with a determination that the subgraphs are
    remergeable. Commit all tentative [Mergeable] relationships recorded by [root]/[expand]. *)

val index_map: t -> (StateNub.Index.t, StateNub.Index.t, StateNub.Index.cmper_witness) Ordmap.t
(** [index_map t] returns a map of remergeable statenub indexes in canonical form.

     Example: Given remergeable set {1, 2, 3}, the map contains [(2, 1); [3, 1)]. *)
