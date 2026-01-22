(** States can become unreachable due to conflict resolution and/or remerging. The state index map
    enables recompression of state indices and isocore set sequence numbers to contiguous integer
    sequences. *)

open! Basis
open! Basis.Rudiments

type t

val init: remaining_state_indexes:(StateIndex.t, StateIndex.cmper_witness) Ordset.t
  -> remergeable_index_map:(StateIndex.t, StateIndex.t, StateIndex.cmper_witness) Ordmap.t
  -> isocores_sn_of_state_index:(StateIndex.t -> uns) -> t
(** [init ~remaining_state_indexes ~remergeable_index_map ~isocores_sn_of_state_index] constructs a
    contiguously numbered mapping of state indices and isocore set sequence numbers, including only
    remaining states and their remergeable aliases (if any). [isocores_sn_of_state_index] returns
    the corresponding state's sequence number within its containing isocore set. *)

val reindexed_state_index: StateIndex.t -> t -> StateIndex.t
(** [reindexed_state_index state_index t] returns the reindexed state index for the original
    [state_index]. The resulting index is always less than or equal to the state's original
     index. *)

val reindexed_state_index_opt: StateIndex.t -> t -> StateIndex.t option
(** [reindexed_state_index state_index t] returns the reindexed state index for the original
    [state_index] if present in the map, [None] otherwise. The resulting index is always less than
    or equal to the state's original index. *)

val reindexed_isocore_set_sn: StateIndex.t -> t -> uns
(** [reindexed_isocore_set_sn state_index t] returns the reindexed isocore set sequence number for
    the original [state_index]. The resulting sequence number is always less than or equal to the
    state's original isocore set sequence number. *)
